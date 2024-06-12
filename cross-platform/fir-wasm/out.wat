(module
  (type (;0;) (func (result f32)))
  (type (;1;) (func (param i32) (result f32)))
  (type (;2;) (func (param i32) (result i32)))
  (type (;3;) (func (param i32 i32) (result i32)))
  (type (;4;) (func))
  (import "functions" "RemoveMe" (func (;0;) (type 0)))
  (import "functions" "AddItem" (func (;1;) (type 1)))
  (import "runtime" "get_variable_object_ref_achr" (func (;2;) (type 2)))
  (import "runtime" "resolve_formid" (func (;3;) (type 3)))
  (func (;4;) (type 4)
    (local i32 i32)
    block ;; label = @1
      i32.const 1
      global.set 0
      return
    end
    block ;; label = @1
      i32.const 0
      call 0
      drop
      block ;; label = @2
        block ;; label = @3
          global.get 0
          i32.const 1
          i32.eq
          br_if 1 (;@2;)
          i32.const 0
          call 2
          i32.const 0
          i32.const 0
          call 3
          i32.const 250
          call 1
          drop
          i32.const 0
          call 0
          drop
        end
      end
      return
    
  )
  (memory (;0;) 1)
  (global (;0;) (mut i32) i32.const 0)
  (export "variable:killme" (global 0))
  (export "strings" (memory 0))
)