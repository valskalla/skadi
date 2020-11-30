<p align="center">
  <img src="skadi.svg" width="1100px" />  
  <br/>
  <i>- The goddess of bowhunting & skiing that traces her prey through the mountains</i>
</p>

----
[![Build Status](https://img.shields.io/github/workflow/status/valskalla/skadi/Scala%20CI)](https://github.com/valskalla/skadi/actions)
[![Maven Central](https://img.shields.io/maven-central/v/com.github.valskalla/skadi-core_2.13)](https://search.maven.org/search?q=com.github.valskalla)
[![Codecov](https://img.shields.io/codecov/c/github/valskalla/skadi)](https://codecov.io/gh/valskalla/skadi)
[![License](https://img.shields.io/github/license/valskalla/skadi)](https://github.com/valskalla/skadi/blob/master/LICENSE)

Skadi is an abstraction over [distributed tracing](https://opentracing.io/docs/overview/what-is-tracing/) that
provides a unified functional API for various tracers with reasoning & usability in mind.

- Compatible with any OpenTracing client library
- Support of span tags & logs
- Each effect is suspended within the polymorphic `F[_]`
- Spans preserved within the context of `F[_]` don't pollute business API

Library heavily relies on cats-effect type classes, and works with _any_ effect that could carry
the context, such as `Kleisli`, `StateT`, `RWST`, `monix.Task` with `TaskLocal`, `ZIO` with `FiberRef`.

# WIP

The library is being battle-tested. Once the results are known, documentation will be complete.