use std::fmt::Debug;

use inkwell::{
    context::Context,
    execution_engine::ExecutionEngine,
    module::{Linkage, Module},
    types::BasicType as _,
    values::{BasicValue, PointerValue},
};

use crate::ADDRESS_SPACE;

pub struct GlobalConstructor<'ctx> {
    priority: u32,
    constructor: PointerValue<'ctx>,
    initialized_value: Option<PointerValue<'ctx>>,
}

pub struct GlobalDestructor<'ctx> {
    priority: u32,
    destructor: PointerValue<'ctx>,
    initialized_value: Option<PointerValue<'ctx>>,
}

pub struct CompilerServices<'ctx> {
    constructors: Vec<GlobalConstructor<'ctx>>,
    destructors: Vec<GlobalDestructor<'ctx>>,
}

impl<'ctx> CompilerServices<'ctx> {
    const fn new() -> Self {
        Self {
            constructors: vec![],
            destructors: vec![],
        }
    }

    pub(crate) fn add_global_construtor(
        &mut self,
        priority: u32,
        constructor: inkwell::values::PointerValue<'ctx>,
        initialized_value: Option<inkwell::values::PointerValue<'ctx>>,
    ) {
        self.constructors.push(GlobalConstructor {
            priority,
            constructor,
            initialized_value,
        });
    }

    #[allow(unused)]
    pub(crate) fn add_global_destructor(
        &mut self,
        priority: u32,
        destructor: PointerValue<'ctx>,
        initialized_value: Option<PointerValue<'ctx>>,
    ) {
        self.destructors.push(GlobalDestructor {
            priority,
            destructor,
            initialized_value,
        });
    }

    pub(crate) fn finalize(self, context: &'ctx Context, module: &Module<'ctx>) {
        Self::finalize_constructors(self.constructors, context, module);
        Self::finalize_destructors(self.destructors, context, module);
    }

    fn finalize_constructors(
        constructors: Vec<GlobalConstructor>,
        context: &'ctx Context,
        module: &Module<'ctx>,
    ) {
        let type_ = context.opaque_struct_type("global_ctors_entry");

        type_.set_body(
            &[
                context.i32_type().as_basic_type_enum(),
                context.ptr_type(*ADDRESS_SPACE).as_basic_type_enum(),
                context.ptr_type(*ADDRESS_SPACE).as_basic_type_enum(),
            ],
            false,
        );

        let global = module.add_global(
            type_
                .array_type(constructors.len().try_into().unwrap())
                .as_basic_type_enum(),
            None,
            "llvm.global_ctors",
        );
        global.set_linkage(Linkage::Appending);

        let array_values = constructors
            .into_iter()
            .map(|x| {
                type_.const_named_struct(&[
                    context
                        .i32_type()
                        .const_int(x.priority.into(), false)
                        .as_basic_value_enum(),
                    x.constructor.as_basic_value_enum(),
                    x.initialized_value
                        .unwrap_or_else(|| context.ptr_type(*ADDRESS_SPACE).const_null())
                        .as_basic_value_enum(),
                ])
            })
            .collect::<Vec<_>>();

        global.set_initializer(&type_.const_array(&array_values).as_basic_value_enum());
    }

    fn finalize_destructors(
        destructors: Vec<GlobalDestructor>,
        context: &'ctx Context,
        module: &Module<'ctx>,
    ) {
        let type_ = context.opaque_struct_type("global_dtors_entry");

        type_.set_body(
            &[
                context.i32_type().as_basic_type_enum(),
                context.ptr_type(*ADDRESS_SPACE).as_basic_type_enum(),
                context.ptr_type(*ADDRESS_SPACE).as_basic_type_enum(),
            ],
            false,
        );

        let global = module.add_global(
            type_
                .array_type(destructors.len().try_into().unwrap())
                .as_basic_type_enum(),
            None,
            "llvm.global_dtors",
        );
        global.set_linkage(Linkage::Appending);

        let array_values = destructors
            .into_iter()
            .map(|x| {
                type_.const_named_struct(&[
                    context
                        .i32_type()
                        .const_int(x.priority.into(), false)
                        .as_basic_value_enum(),
                    x.destructor.as_basic_value_enum(),
                    x.initialized_value
                        .unwrap_or_else(|| context.ptr_type(*ADDRESS_SPACE).const_null())
                        .as_basic_value_enum(),
                ])
            })
            .collect::<Vec<_>>();

        global.set_initializer(&type_.const_array(&array_values).as_basic_value_enum());
    }
}

pub struct ModuleCompiler<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    compiler_services: CompilerServices<'ctx>,
}

impl<'ctx> ModuleCompiler<'ctx> {
    pub(super) const fn new(context: &'ctx Context, module: Module<'ctx>) -> Self {
        Self {
            context,
            module,
            compiler_services: CompilerServices::new(),
        }
    }

    pub(crate) fn build<TReturn>(
        &mut self,
        compile_insides: impl FnOnce(
            &'ctx Context,
            &Module<'ctx>,
            &mut CompilerServices<'ctx>,
        ) -> TReturn,
    ) -> TReturn {
        compile_insides(self.context, &self.module, &mut self.compiler_services)
    }

    pub(crate) fn run(self, action: impl Fn(&ExecutionEngine<'ctx>)) {
        self.compiler_services.finalize(self.context, &self.module);
        self.module.verify().unwrap();

        let execution_engine = self
            .module
            // TODO ensure that the allocator is the same one that is used by Rust, we might need
            // to switch to `create_mcjit_execution_engine_with_memory_manager` and pass out own
            // allocator
            .create_jit_execution_engine(inkwell::OptimizationLevel::Default)
            .unwrap();

        // TODO this is a hack, we should provide the global mappings elsewhere
        execution_engine.add_global_mapping(
            &self.module.get_function("println").unwrap(),
            printline as usize,
        );

        execution_engine.run_static_constructors();
        action(&execution_engine);
        execution_engine.run_static_destructors();
    }
}

// TODO move this to stdlib
extern "C" fn printline() {
    println!("ok");
}

impl Debug for ModuleCompiler<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            &self
                .module
                .print_to_string()
                .to_string()
                .replace("\\n", "\n"),
        )
    }
}
