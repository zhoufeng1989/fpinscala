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



###using option with exceptions###
A common idiom is to do o.getOrElse(throw new Exception("FAIL")) to con-
vert the None case of an Option back to an exception. The general rule of thumb is
that we use exceptions only if no reasonable program would ever catch the exception;
if for some callers the exception might be a recoverable error, we use Option (or
Either) to give them flexibility.


###Option composition, lifting, and wrapping exception-oriented APIs###
It may be easy to jump to the conclusion that once we start using Option , it infects our
entire code base. One can imagine how any callers of methods that take or return
Option will have to be modified to handle either Some or None . But this doesn’t hap-
pen, and the reason is that we can lift ordinary functions to become functions that
operate on Option .


#Strict and non-strict functions#

###strict and no-strict functions###
Non-strictness is a property of a function. To say a function is non-strict just means
that the function may choose not to evaluate one or more of its arguments. In con-
trast, a strict function always evaluates its arguments. Strict functions are the norm in
most programming languages, and indeed most languages only support functions
that expect their arguments fully evaluated.

As a final bit of terminology, we say that a non-strict function in Scala takes its argu-
ments by name rather than by value.

def x = 10
def y() = 10
val z = () => 10


#algebras#
considering data types in terms of their algebras—that is, the operations they support and the laws that gov-
ern those operations.

#monoid#

a monoid is a type together with a binary operation ( op ) over that type, satisfying associativity and having an identity element ( zero )
