use std::ffi::CStr;

#[repr(u8)]
#[allow(unused)]
pub enum ObjectFieldKind {
    U8 = 1,
    Object = 64,
    Callable = 128,
}

#[allow(unused)]
pub union FieldValue {
    pub(super) u64: u64,
    pub(super) object: *mut Object,
    pub(super) callable: fn(),
}

#[repr(C)]
pub struct Field {
    pub(super) name: u64,
    pub(super) value: FieldValue,
    pub(super) value_kind: ObjectFieldKind,
}

#[repr(C)]
pub struct ImplementedInterface {
    pub(super) name: *const CStr,
    pub(super) interface_definition: *mut Object,
}

#[repr(C)]
#[allow(unused)] // TODO use it 
pub struct Object {
    pub(super) fields: *mut Field,
    pub(super) field_count: u64,
    pub(super) interfaces: *mut ImplementedInterface,
    pub(super) interface_count: u64,

    pub(super) type_: *mut Object,
}
