#functional data structures.#

###data sharing###
Sharing of immutable data often lets us implement functions more efficiently;
we can always return immutable data structures without having to worry about subse-
quent code modifying our data. There’s no need to pessimistically make copies to
avoid modification or corruption.


#Handling errors without exceptions.#

###referencial transparency###
the meaning of RT expressions does not depend on context and may be reasoned about locally, 
whereas the meaning of non- RT expressions is context-dependent and requires more global reasoning.
For instance, the meaning of the RT expression 42 + 5 doesn’t depend on the larger expression it’s embedded
in—it’s always and forever equal to 47 . But the meaning of the expression throw new
Exception("fail") is very context-dependent, it takes on different meanings depending on which try block (if any) 
it’s nested within.

###There are two main problems with exceptions###
Exceptions break RT and introduce context dependence, moving us away from the simple reasoning of the substitution model 
and making it possible to write confusing exception-based code. This is the source of the folklore advice
that exceptions should be used only for error handling, not for control flow.

Exceptions are not type-safe.For example, Int => Int tells us nothing
about the fact that exceptions may occur, and the compiler will certainly not
force callers of the function to make a decision about how to handle those excep-
tions. If we forget to check for an exception in failingFn , this won’t be detected
until runtime.

###checked exception in java###
Java’s checked exceptions at least force a decision about whether to handle or
reraise an error, but they result in significant boilerplate for callers. More importantly,
they don’t work for higher-order functions, which can’t possibly be aware of the spe-
cific exceptions that could be raised by their arguments.
This is why generic code, even in Java, so often resorts to using RuntimeException or some 
common checked Exception type.

###alternative to exceptions###
We’d like an alternative to exceptions without these drawbacks, but we don’t want to
lose out on the primary benefit of exceptions: they allow us to consolidate and centralize
error-handling logic, rather than being forced to distribute this logic throughout our
codebase. The technique we use is based on an old idea: instead of throwing an excep-
tion, we return a value indicating that an exceptional condition has occurred. This
idea might be familiar to anyone who has used return codes in C to handle excep-
tions. But instead of using error codes, we introduce a new generic type for these “pos-
sibly defined values” and use higher-order functions to encapsulate common patterns
of handling and propagating errors. Unlike C-style error codes, the error-handling
strategy we use is completely type-safe, and we get full assistance from the type-checker in
forcing us to deal with errors, with a minimum of syntactic noise.




