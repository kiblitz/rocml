open! Core
open! Import
open Ppxlib

let kernel_attribute = "kernel"

module Location = struct
  module Position = struct
    type t =
      { filename : string
      ; line_number : int
      ; column_number : int
      }
    [@@deriving sexp_of]

    let of_position ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : position) =
      { filename = pos_fname; line_number = pos_lnum; column_number = pos_cnum - pos_bol }
    ;;
  end

  type t =
    { start : Position.t
    ; end_ : Position.t
    }
  [@@deriving sexp_of]

  let of_location ({ loc_start; loc_end; loc_ghost = _ } : location) =
    { start = Position.of_position loc_start; end_ = Position.of_position loc_end }
  ;;
end

module Kernelize : sig
  module Mode : sig
    type t =
      | Prod
      | Debug_string

    val merge : t -> t -> t Or_error.t
  end

  val generate_ast : expression -> Kernel_ast.t
end = struct
  module Mode = struct
    type t =
      | Prod
      | Debug_string
    [@@deriving equal, sexp_of]

    let merge t1 t2 =
      if equal t1 t2
      then Ok t1
      else error_s [%message "Cannot merge different kernelize modes" (t1 : t) (t2 : t)]
    ;;
  end

  let rec on_expression expression =
    let raise_on_unexpected_expression kind =
      let location = Location.of_location expression.pexp_loc in
      raise_s
        [%message "Cannot kernelize expression" (kind : string) (location : Location.t)]
    in
    match expression.pexp_desc with
    (* Ident *)
    | Pexp_ident { txt = Lident ident; loc = _ } ->
      Kernel_ast.Expression.Identifier { name = ident }
    | Pexp_ident _ -> raise_on_unexpected_expression "Non_ident_var"
    (* Const *)
    | Pexp_constant (Pconst_integer (int_, None)) ->
      Kernel_ast.Expression.Constant (Int (Int.of_string int_))
    | Pexp_constant (Pconst_float (float_, None)) ->
      Kernel_ast.Expression.Constant (Float (Float.of_string float_))
    | Pexp_constant _ -> raise_on_unexpected_expression "Non_raw_constant_int_or_float"
    | Pexp_construct ({ txt = Lident "true"; _ }, None) ->
      Kernel_ast.Expression.Constant (Bool true)
    | Pexp_construct ({ txt = Lident "false"; _ }, None) ->
      Kernel_ast.Expression.Constant (Bool false)
    (* Assign *)
    | Pexp_let (Nonrecursive, value_bindings, within_scope) ->
      let assignments =
        let%map.List { pvb_pat; pvb_expr; _ } = value_bindings in
        let ident =
          match pvb_pat.ppat_desc with
          | Ppat_var { txt; _ } -> { Kernel_ast.Identifier.name = txt }
          | _ -> raise_on_unexpected_expression "Non_ident_var_assignment"
        in
        let value = on_expression pvb_expr in
        { Kernel_ast.Assignment.ident; value }
      in
      let within_scope = on_expression within_scope in
      Kernel_ast.Expression.Assign { assignments; within_scope }
    | Pexp_let (Recursive, _, _) -> raise_on_unexpected_expression "Recursive_assignment"
    (* Apply *)
    | Pexp_apply (func, args) ->
      let f = on_expression func in
      let args =
        let%map.List arg_type, expression = args in
        let arg = on_expression expression in
        let label =
          match arg_type with
          | Nolabel -> Kernel_ast.Arg.Label.Unnamed
          | Labelled name -> Labelled name
          | Optional name -> Optional name
        in
        { Kernel_ast.Arg.label; expression = arg }
      in
      Kernel_ast.Expression.Apply { f; args }
    (* If-then-else *)
    | Pexp_ifthenelse (condition, then_, else_) ->
      let condition = on_expression condition in
      let then_ = on_expression then_ in
      let else_ = Option.map else_ ~f:on_expression in
      Kernel_ast.Expression.If_then_else { condition; then_; else_ }
    (* For *)
    | Pexp_for ({ ppat_desc = Ppat_var { txt; _ }; _ }, from, to_, direction_flag, body)
      ->
      let ident = { Kernel_ast.Identifier.name = txt } in
      let from = on_expression from in
      let to_ = on_expression to_ in
      let body = on_expression body in
      let direction =
        match direction_flag with
        | Upto -> Kernel_ast.Direction.Up
        | Downto -> Down
      in
      Kernel_ast.Expression.For { ident; from; to_; direction; body }
    | Pexp_for _ -> raise_on_unexpected_expression "Non_var_for_iterator"
    (* While *)
    | Pexp_while (condition, body) ->
      let condition = on_expression condition in
      let body = on_expression body in
      Kernel_ast.Expression.While { condition; body }
    (* Sequence *)
    | Pexp_sequence (this, next) ->
      let this = on_expression this in
      let next = on_expression next in
      Kernel_ast.Expression.Sequence { this; next }
    (* Not part of syntax *)
    | Pexp_function _ -> raise_on_unexpected_expression "Function_pattern_match"
    | Pexp_fun _ -> raise_on_unexpected_expression "Lambda"
    | Pexp_match _ -> raise_on_unexpected_expression "Pattern_match"
    | Pexp_try _ -> raise_on_unexpected_expression "Try"
    | Pexp_tuple _ -> raise_on_unexpected_expression "Tuple"
    | Pexp_construct _ -> raise_on_unexpected_expression "Construct"
    | Pexp_variant _ -> raise_on_unexpected_expression "Variant"
    | Pexp_record _ -> raise_on_unexpected_expression "Record"
    | Pexp_field _ -> raise_on_unexpected_expression "Field"
    | Pexp_setfield _ -> raise_on_unexpected_expression "Set_field"
    | Pexp_array _ -> raise_on_unexpected_expression "Array_define"
    | Pexp_constraint _ -> raise_on_unexpected_expression "Type_constraint"
    | Pexp_coerce _ -> raise_on_unexpected_expression "Type_coerce"
    | Pexp_send _ -> raise_on_unexpected_expression "Method_call"
    | Pexp_new _ -> raise_on_unexpected_expression "New"
    | Pexp_setinstvar _ -> raise_on_unexpected_expression "Set_class_var"
    | Pexp_override _ -> raise_on_unexpected_expression "Override"
    | Pexp_letmodule _ -> raise_on_unexpected_expression "Assign_module"
    | Pexp_letexception _ -> raise_on_unexpected_expression "Assign_exception"
    | Pexp_assert _ -> raise_on_unexpected_expression "Assert"
    | Pexp_lazy _ -> raise_on_unexpected_expression "Lazy"
    | Pexp_poly _ -> raise_on_unexpected_expression "Method_body"
    | Pexp_object _ -> raise_on_unexpected_expression "Object"
    | Pexp_newtype _ -> raise_on_unexpected_expression "New_type"
    | Pexp_pack _ -> raise_on_unexpected_expression "Pack"
    | Pexp_open _ -> raise_on_unexpected_expression "Open"
    | Pexp_letop _ -> raise_on_unexpected_expression "Let_op"
    | Pexp_extension _ -> raise_on_unexpected_expression "Extension"
    | Pexp_unreachable -> raise_on_unexpected_expression "Unreachable"
  ;;

  let rec extract_func_arg_name pattern =
    let raise_on_unexpected_function_param_name kind =
      let location = Location.of_location pattern.ppat_loc in
      raise_s
        [%message
          "Cannot extract function arg name" (kind : string) (location : Location.t)]
    in
    match pattern.ppat_desc with
    | Ppat_var { txt; _ } -> txt
    | Ppat_alias (pattern, _) -> extract_func_arg_name pattern
    | Ppat_constraint (pattern, _) -> extract_func_arg_name pattern
    | Ppat_any -> raise_on_unexpected_function_param_name "Empty"
    | Ppat_constant _ -> raise_on_unexpected_function_param_name "Constant"
    | Ppat_interval _ -> raise_on_unexpected_function_param_name "Interval"
    | Ppat_tuple _ -> raise_on_unexpected_function_param_name "Tuple"
    | Ppat_construct _ -> raise_on_unexpected_function_param_name "Construct"
    | Ppat_variant _ -> raise_on_unexpected_function_param_name "Variant"
    | Ppat_record _ -> raise_on_unexpected_function_param_name "Record"
    | Ppat_array _ -> raise_on_unexpected_function_param_name "Array"
    | Ppat_or _ -> raise_on_unexpected_function_param_name "Or"
    | Ppat_type _ -> raise_on_unexpected_function_param_name "Type"
    | Ppat_lazy _ -> raise_on_unexpected_function_param_name "Lazy"
    | Ppat_unpack _ -> raise_on_unexpected_function_param_name "Unpack"
    | Ppat_exception _ -> raise_on_unexpected_function_param_name "Exception"
    | Ppat_extension _ -> raise_on_unexpected_function_param_name "Extension"
    | Ppat_open _ -> raise_on_unexpected_function_param_name "Open"
  ;;

  let rec on_function ?(func_args = []) expression =
    let raise_on_unexpected_function_param kind =
      let location = Location.of_location expression.pexp_loc in
      raise_s
        [%message
          "Cannot kernelize function param" (kind : string) (location : Location.t)]
    in
    match expression.pexp_desc with
    | Pexp_fun (Nolabel, None, func_arg, body) ->
      let name = extract_func_arg_name func_arg in
      let func_args = { Kernel_ast.Identifier.name } :: func_args in
      on_function ~func_args body
    | Pexp_fun (Labelled _, _, _, _) ->
      raise_on_unexpected_function_param "Labelled_param"
    | Pexp_fun (Optional _, _, _, _) ->
      raise_on_unexpected_function_param "Optional_param"
    | Pexp_fun
        (_, _, { ppat_desc = Ppat_constraint ({ ppat_desc = Ppat_var _; _ }, _); _ }, _)
      -> raise_on_unexpected_function_param "Non_gpu_type"
    | Pexp_fun (_, _, { ppat_desc = Ppat_constraint _; _ }, _) ->
      raise_on_unexpected_function_param "Non_var_argument"
    | Pexp_fun (_, _, _, _) -> raise_on_unexpected_function_param "Untyped_argument"
    | (_body : expression_desc) ->
      { Kernel_ast.func_args = List.rev func_args; body = on_expression expression }
  ;;

  let generate_ast expression = on_function expression
end

let has_kernelize_attribute value_binding =
  value_binding.pvb_attributes
  |> List.filter ~f:(fun attribute ->
    [%equal: string] attribute.attr_name.txt kernel_attribute)
  |> List.map ~f:(fun attribute ->
    match attribute.attr_payload with
    | PStr [] -> Kernelize.Mode.Prod
    | PStr
        [ { pstr_desc =
              Pstr_eval
                ({ pexp_desc = Pexp_ident { txt = Lident "debug_string"; _ }; _ }, _)
          ; _
          }
        ] -> Debug_string
    | _ -> raise_s [%message "Expected either no attribute payload or [debug_string]"])
  |> List.reduce ~f:(fun a1 a2 -> Kernelize.Mode.merge a1 a2 |> Or_error.ok_exn)
;;

let rec transform_structure_item structure_item =
  let pstr_desc =
    match structure_item.pstr_desc with
    | Pstr_value (Nonrecursive, value_bindings) ->
      Pstr_value (Nonrecursive, List.map value_bindings ~f:transform_value_binding)
    | Pstr_module module_binding ->
      Pstr_module
        { module_binding with pmb_expr = transform_module_expr module_binding.pmb_expr }
    | Pstr_eval (expression, attributes) ->
      Pstr_eval (transform_expression expression, attributes)
    | Pstr_recmodule module_bindings ->
      Pstr_recmodule
        (List.map module_bindings ~f:(fun module_binding ->
           { module_binding with
             pmb_expr = transform_module_expr module_binding.pmb_expr
           }))
    | pstr_desc -> pstr_desc
  in
  { structure_item with pstr_desc }

and transform_value_binding value_binding =
  match has_kernelize_attribute value_binding with
  | Some mode ->
    let kernel_ast = Kernelize.generate_ast value_binding.pvb_expr in
    let pvb_expr =
      match mode with
      | Prod -> value_binding.pvb_expr
      | Debug_string ->
        let pexp_desc =
          let kernel_ast_string =
            [%sexp_of: Kernel_ast.t] kernel_ast |> Sexp.to_string_hum
          in
          Pexp_constant (Pconst_string (kernel_ast_string, value_binding.pvb_loc, None))
        in
        { value_binding.pvb_expr with pexp_desc }
    in
    { value_binding with pvb_expr }
  | None ->
    let pvb_expr = transform_expression value_binding.pvb_expr in
    { value_binding with pvb_expr }

and transform_expression expression =
  let pexp_desc =
    match expression.pexp_desc with
    | Pexp_let (Nonrecursive, value_bindings, within_scope) ->
      Pexp_let
        ( Nonrecursive
        , List.map value_bindings ~f:transform_value_binding
        , transform_expression within_scope )
    | Pexp_function cases -> Pexp_function (List.map cases ~f:transform_case)
    | Pexp_fun (arg_label, default_arg_value, pattern, body) ->
      Pexp_fun
        ( arg_label
        , Option.map default_arg_value ~f:transform_expression
        , pattern
        , transform_expression body )
    | Pexp_apply (func, param_list) ->
      Pexp_apply
        ( transform_expression func
        , let%map.List arg_label, value = param_list in
          arg_label, transform_expression value )
    | Pexp_match (condition, cases) ->
      Pexp_match (transform_expression condition, List.map cases ~f:transform_case)
    | Pexp_try (condition, cases) ->
      Pexp_try (transform_expression condition, List.map cases ~f:transform_case)
    | Pexp_tuple values -> Pexp_tuple (List.map values ~f:transform_expression)
    | Pexp_construct (ident, expression) ->
      Pexp_construct (ident, Option.map expression ~f:transform_expression)
    | Pexp_variant (label, expression) ->
      Pexp_variant (label, Option.map expression ~f:transform_expression)
    | Pexp_record (fields, when_) ->
      Pexp_record
        ( (let%map.List ident, assignment = fields in
           ident, transform_expression assignment)
        , Option.map when_ ~f:transform_expression )
    | Pexp_field (expression, ident) -> Pexp_field (transform_expression expression, ident)
    | Pexp_setfield (expression, ident, assignment) ->
      Pexp_setfield
        (transform_expression expression, ident, transform_expression assignment)
    | Pexp_array items -> Pexp_array (List.map items ~f:transform_expression)
    | Pexp_ifthenelse (condition, then_, else_) ->
      Pexp_ifthenelse
        ( transform_expression condition
        , transform_expression then_
        , Option.map else_ ~f:transform_expression )
    | Pexp_sequence (this, next) ->
      Pexp_sequence (transform_expression this, transform_expression next)
    | Pexp_while (condition, body) ->
      Pexp_while (transform_expression condition, transform_expression body)
    | Pexp_for (pattern, from_, to_, direction_flag, body) ->
      Pexp_for
        ( pattern
        , transform_expression from_
        , transform_expression to_
        , direction_flag
        , transform_expression body )
    | Pexp_constraint (expression, core_type) ->
      Pexp_constraint (transform_expression expression, core_type)
    | Pexp_coerce (expression, core_type, coercion) ->
      Pexp_coerce (transform_expression expression, core_type, coercion)
    | Pexp_send (expression, ident) -> Pexp_send (transform_expression expression, ident)
    | Pexp_setinstvar (ident, assignment) ->
      Pexp_setinstvar (ident, transform_expression assignment)
    | Pexp_override overrides ->
      Pexp_override
        (let%map.List ident, override = overrides in
         ident, transform_expression override)
    | Pexp_letmodule (module_name, module_expr, within_scope) ->
      Pexp_letmodule
        (module_name, transform_module_expr module_expr, transform_expression within_scope)
    | Pexp_letexception (extension_constructor, within_scope) ->
      Pexp_letexception (extension_constructor, transform_expression within_scope)
    | Pexp_assert assertion -> Pexp_assert (transform_expression assertion)
    | Pexp_lazy expression -> Pexp_lazy (transform_expression expression)
    | Pexp_poly (expression, core_type) ->
      Pexp_poly (transform_expression expression, core_type)
    | Pexp_object class_structure ->
      Pexp_object
        { class_structure with
          pcstr_fields = List.map class_structure.pcstr_fields ~f:transform_class_field
        }
    | Pexp_newtype (type_, body) -> Pexp_newtype (type_, transform_expression body)
    | Pexp_pack module_expr -> Pexp_pack (transform_module_expr module_expr)
    | Pexp_open (open_declaration, within_scope) ->
      Pexp_open (open_declaration, transform_expression within_scope)
    | Pexp_letop { let_; ands; body } ->
      let transform_binding_op binding_op =
        { binding_op with pbop_exp = transform_expression binding_op.pbop_exp }
      in
      Pexp_letop
        { let_ = transform_binding_op let_
        ; ands = List.map ands ~f:transform_binding_op
        ; body = transform_expression body
        }
    | expression -> expression
  in
  { expression with pexp_desc }

and transform_case case =
  let pc_guard =
    let%map.Option pc_guard = case.pc_guard in
    transform_expression pc_guard
  in
  let pc_rhs = transform_expression case.pc_rhs in
  { case with pc_guard; pc_rhs }

and transform_module_expr module_expr =
  let pmod_desc =
    match module_expr.pmod_desc with
    | Pmod_structure structure ->
      Pmod_structure (List.map structure ~f:transform_structure_item)
    | Pmod_functor (functor_parameter, module_expr) ->
      Pmod_functor (functor_parameter, transform_module_expr module_expr)
    | Pmod_apply (module_expr1, module_expr2) ->
      Pmod_apply (transform_module_expr module_expr1, transform_module_expr module_expr2)
    | Pmod_constraint (module_expr, module_type) ->
      Pmod_constraint
        (transform_module_expr module_expr, transform_module_type module_type)
    | Pmod_unpack expression -> Pmod_unpack (transform_expression expression)
    | pmod_desc -> pmod_desc
  in
  { module_expr with pmod_desc }

and transform_module_type module_type =
  let pmty_desc =
    match module_type.pmty_desc with
    | Pmty_functor (functor_parameter, module_type) ->
      Pmty_functor (functor_parameter, transform_module_type module_type)
    | Pmty_with (module_type, with_constraints) ->
      Pmty_with (transform_module_type module_type, with_constraints)
    | Pmty_typeof module_expr -> Pmty_typeof (transform_module_expr module_expr)
    | pmty_desc -> pmty_desc
  in
  { module_type with pmty_desc }

and transform_class_field class_field =
  let pcf_desc =
    match class_field.pcf_desc with
    | Pcf_inherit (override_flag, class_expression, alias) ->
      Pcf_inherit (override_flag, transform_class_expression class_expression, alias)
    | Pcf_initializer expression -> Pcf_initializer (transform_expression expression)
    | pcf_desc -> pcf_desc
  in
  { class_field with pcf_desc }

and transform_class_expression class_expression =
  let pcl_desc =
    match class_expression.pcl_desc with
    | Pcl_structure class_structure ->
      Pcl_structure
        { class_structure with
          pcstr_fields = List.map class_structure.pcstr_fields ~f:transform_class_field
        }
    | Pcl_fun (arg_label, default_assignment, pattern, class_expr) ->
      Pcl_fun
        ( arg_label
        , Option.map default_assignment ~f:transform_expression
        , pattern
        , transform_class_expression class_expr )
    | Pcl_apply (class_expr, assignments) ->
      Pcl_apply
        ( transform_class_expression class_expr
        , let%map.List arg_label, assignment = assignments in
          arg_label, transform_expression assignment )
    | Pcl_let (Nonrecursive, value_bindings, class_expression) ->
      Pcl_let
        ( Nonrecursive
        , List.map value_bindings ~f:transform_value_binding
        , transform_class_expression class_expression )
    | Pcl_constraint (class_expression, class_type) ->
      Pcl_constraint (transform_class_expression class_expression, class_type)
    | Pcl_open (open_description, class_expression) ->
      Pcl_open (open_description, transform_class_expression class_expression)
    | pcl_desc -> pcl_desc
  in
  { class_expression with pcl_desc }
;;

let transform_structure structure = List.map structure ~f:transform_structure_item
let () = Driver.register_transformation kernel_attribute ~impl:transform_structure
