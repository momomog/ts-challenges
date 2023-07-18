import type { Equal } from '@type-challenges/utils'

// 1 Pick
type MyPick<T, K extends keyof T> = {
  [Prop in K]: T[Prop]
}

// 2 Readonly
type MyReadonly<T> = {
  readonly [Prop in keyof T]: T[Prop]
}

// 3 Tuple to Object
type TupleToObject<T extends readonly any[]> = {
  [Prop in T[number]]: Prop
}

//4 First of Array
type First<T extends any[]> = T extends [] ? never : T[0]

// 5 Length of Tuple
type Length<T extends any> = T extends { length: number } ? T['length'] : never

// 6 Exclude
type MyExclude<T, U> = T extends U ? never : T

// 7 Awaited
type MyAwaited<T> =
  T extends Promise<infer R> ? R extends Promise<any> ? MyAwaited<R> : R
    : T extends { then: (arg: (ar: infer Y) => any) => any } ? Y : never

// 8 If
type If<C, T, F> = C extends true ? T : F

// 9 Concat
type Concat<T extends any[], U extends any[]> = [...T, ...U]

// 10 Includes
type IsEqual2<T, U> = Equal<T, U> extends true ? true : false

type Includes<T extends readonly any[], U> = T extends [infer First, ...infer Rest] ? IsEqual2<First, U> extends true ? true : Includes<Rest, U> : false

// 11 Push
type Push<T extends any[], U> = [...T, U]

// 12 Unshift
type Unshift<T extends any[], U> = [U, ...T]

// 13 MyParameters
type MyParameters<T> = T extends (...args: infer P) => any ? P : never
