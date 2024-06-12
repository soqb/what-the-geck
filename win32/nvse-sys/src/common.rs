use core::slice;
use std::{
    borrow::Cow,
    ffi::{c_char, CStr},
    marker::PhantomData,
    mem::transmute,
};

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct WinStr<'a> {
    ptr: *const c_char,
    _marker: PhantomData<&'a ()>,
}

impl<'a> WinStr<'a> {
    /// Wraps a raw C-style string pointer with a safe C string wrapper.
    ///
    /// # Safety
    /// No additional safety invariants are imposed beyond what is required by [`Cstr::from_ptr`].
    ///
    /// And additionally, `ptr` is allowed to be null.
    pub unsafe fn from_ptr(ptr: *const c_char) -> Self {
        Self {
            ptr,
            _marker: PhantomData,
        }
    }

    pub fn as_c_str(self) -> &'a CStr {
        if self.ptr.is_null() {
            return <&CStr>::default();
        }
        // SAFETY: invariants are upheld by construction.
        unsafe { CStr::from_ptr(self.ptr) }
    }

    pub fn to_bytes(self) -> &'a [u8] {
        self.as_c_str().to_bytes()
    }

    pub fn decode(self) -> Cow<'a, str> {
        encoding_rs::WINDOWS_1252.decode(self.to_bytes()).0
    }
}

#[repr(C)]
pub struct WinString {
    ptr: *mut c_char,
    data_len: u16,
    buf_len: u16,
}

impl WinString {
    pub fn as_win_str(&self) -> WinStr<'_> {
        // data can
        // SAFETY: lifetimes are sound, and requirements on `buf` and the `ptr` parameter are the same.
        unsafe { WinStr::from_ptr(self.ptr) }
    }

    pub fn to_bytes(&self) -> &[u8] {
        // SAFETY: `ptr` is owned for at least `data_len + 1` bytes.
        unsafe { slice::from_raw_parts(self.ptr as *const u8, self.data_len.into()) }
    }

    pub fn decode(&self) -> Cow<'_, str> {
        encoding_rs::WINDOWS_1252.decode(self.to_bytes()).0
    }

    pub fn to_vec(self) -> Vec<u8> {
        // SAFETY: `buf` is mandated to last at least `data_len` bytes.
        unsafe {
            Vec::from_raw_parts(
                self.ptr as *mut u8,
                self.data_len.into(),
                self.buf_len.into(),
            )
        }
    }
}

impl Drop for WinString {
    fn drop(&mut self) {
        unsafe {
            Vec::from_raw_parts(
                self.ptr as *mut u8,
                self.data_len.into(),
                self.buf_len.into(),
            );
        }
    }
}

#[repr(packed)]
pub struct VtableHeader {
    // todo: more info needed.
    meta: Unknown,
    null: Unknown,
}

#[repr(packed)]
pub struct Vtable<T> {
    _hdr: VtableHeader,
    pub table: T,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Unknown(*const ());

impl Unknown {
    pub fn ptr(&self) -> *const () {
        self.0
    }
}

#[repr(C)]
struct ListNode {
    pub data: *mut (),
    pub next: *mut ListNode,
}

/// A strange linked list.
#[repr(C)]
pub struct TList<T> {
    head: ListNode,
    _marker: PhantomData<T>,
}
