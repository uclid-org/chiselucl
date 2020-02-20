# Development Notes

## Failing to Produce Any UCLID
AsyncResetTester.fir - 2 Clocks, 1 Explicit reset
CustomTransform.fir - Unsupported modules
HasLoops.fir - Has combinational loops (Don't need to worry about this)
LargeParamTester.fir - Nested circuit, external modules
MultiExtModuleTester - Nested circuit
ParameterizedExtModuleTester.fir - Nested circuit
RenamedExtModuleTester.fir - Nested circuit
Rob.fir - Multiple clock domains
RocketCore.fir - Nested circuit
SimpleExtModuleTester.fir - Nested circuit

## Failing to Produce Correct UCLID
RightShiftTester.fir - This might have some issues since we aren't intelligently handling asInt and asUInt operators

## Language Level Semantics:
- How do we encode `stop`? Do we even need to to encode them since they are used for simulation purposes? Do we treat them like 'debug' statements, like `printf`
- TODO: Need to talk to Albert about how low we want to transform the FIRRTL. In other words which semantics to model in UCLID5.

## Notes
- Observation: The current version of the tool, treats a pure clock signal and one that combines different the pure clock and other input as two different signals. This can be resolved by recursively constructing the expressions of the pure clock and making state changes within the `next` block based off of the mixed signal.

- Issue: It seems that the renaming for register updates has a bug in it. There is an UnknownType that is generated for simple circuits (see DecoupledGCD.fir). 

- TODO: Double check how we treat signed literals.
  - The examples affected by this are Legalize and HwachaSequencer

- Potetential Fix: We need a way of treating `ValidIf` expressions. One idea is to introduce a fresh variable that is kept as the 'else' case. Since  `ValidIf` expressions are not restricted to assignment statements, we would need to augment UCLID5 to directly represent the notion of undefined (havoc is not sufficient). Another thing to consider, is that can we eliminate `ValidIf` expression with the pre-defined FIRRTL pass.

