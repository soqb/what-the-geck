// #[cxx::bridge]
// mod ffi {
//     unsafe extern "C++" {
//         include!("NVSE/nvse/nvse/prefix.h");
//         include!("NVSE/nvse/nvse/GameForms.h");

//         type TESForm;

//         unsafe fn Destroy(self: Pin<&mut TESForm>, do_free: bool) -> *mut TESForm;
//     }
// }

autocxx::include_cpp! {
    #include "nvse/prefix.h"
    safety!(unsafe_ffi)
}
