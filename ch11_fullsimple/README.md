# Typed Lamda Calculus (with varios extensions)

## Syntax

### Terms

```
t ::=
    x
    l x:T. t
    t t
    true
    false
    if t then t else t
```

### Values

```
v ::=
    x
    l x:T. t
    true
    false
```

### Types

```
T ::=
    Bool
    T -> T
```

### Contexts

```
Γ ::=
    Φ
    Γ, x:T
```

## Typing Rules

TODO

## Evaluation Rules

TODO
