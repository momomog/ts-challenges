import type { Equal } from '@type-challenges/utils'

//   6 - Simple Vue
type GetReturnType<T> = {
  [Prop in keyof T] : T[Prop] extends () => infer R ? R : never
}

type Options<D, C, M> = {
  data: () => D
  computed: C
  methods: M
} & ThisType<D & GetReturnType<C> & M>

declare function SimpleVue<D, C, M>(options: Options<D, C, M>): any

//  57 - Get Required
type GetRequired<T> = {
  [Prop in keyof T as T[Prop] extends Required<T>[Prop] ? Prop : never]: T[Prop]
}

//  59 - Get Optional
type GetOptional<T> = {
  [Prop in keyof T as T[Prop] extends Required<T>[Prop] ? never : Prop] : T[Prop]
}

//  89 - Required Keys
type RequiredKeys<T> = keyof {
  [Prop in keyof T as T[Prop] extends Required<T>[Prop] ? Prop : never] : T[Prop]
}

//  90 - Optional Keys
type OptionalKeys<T> = keyof {
  [Prop in keyof T as T[Prop] extends Required<T>[Prop] ? never : Prop] : T[Prop]
}

//  112 - Capitalize Words
type CapitalizeWords<
  S extends string,
  Res extends string = ''
> = S extends `${infer First}${infer Rest}`
  ? Uppercase<First> extends Lowercase<First> ? CapitalizeWords<Capitalize<Rest>, `${Res}${First}`> : CapitalizeWords<Rest, `${Res}${First}`>
  : Capitalize<Res>

// 114 - CamelCase
type IsSymbol<T extends string> = Lowercase<T> extends Uppercase<T> ? true : false

type CamelCase<S extends string, Res extends string = ''> = S extends `${infer First}${infer Rest}`
  ? IsSymbol<First> extends true
    ? CamelCase<Rest, `${Res}${First}`>
    : Res extends `${infer Prefix}_`
      ? CamelCase<Rest,`${Prefix}${Uppercase<First>}`>
      : CamelCase<Rest, `${Res}${Lowercase<First>}`>
  : Res

//  147 - C-printf Parser
type ParsePrintFormat<T extends string, Res extends any[] = []> =
  T extends `${infer Start}%${infer Sym}${infer Rest}`
    ? Sym extends keyof ControlsMap ? ParsePrintFormat<Rest, [...Res, ControlsMap[Sym]]> : ParsePrintFormat<Rest, Res>
    : Res

//  223 - IsAny
type IsAny<T> = Equal<T, any> extends true ? true : false

//  270 - Typed Get
type Get<T, K> = K extends keyof T
  ? T[K] : K extends `${infer First}.${infer Rest}`
    ? First extends keyof T ? Get<T[First], Rest> : never
    : never

//  300 - String to Number
type Digits = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'

type IsNotValidNumber<T extends string> = T extends `${infer First}${infer Rest}` ? First extends Digits ? IsNotValidNumber<Rest> : true : false

type ToNumber<S extends string, Res extends any[] = []>
  = IsNotValidNumber<S> extends true ? never : `${Res['length']}` extends S ? Res['length'] : ToNumber<S, [...Res, 0]>

//  651 - Length of String 2
type LengthOfString2<S extends string, Res extends string[] = []> = S extends `${infer First}${infer Rest}` ? LengthOfString2<Rest, [...Res, '0']> : Res['length']

// 847 - String Join
type Join<D extends string, T extends readonly string[], Res extends string = ''>
  = T extends [infer First extends string, ...infer Rest extends string[]] ? Join<D, Rest, `${Res}${Res extends '' ? '' : D}${First}`> : Res

type Result<D extends string, P extends readonly string[]> =
  P['length'] extends 0 ? ''
    : P['length'] extends 1 ? P[0]
      : Join<D, P>

declare function join<D extends string>(delimiter: D): <P extends readonly string[]>(...parts: P) => Result<D, P>

//   2059 - Drop String
type IsIncludes<S extends string, Sym extends string> = S extends `${infer First}${infer Rest}` ? First extends Sym ? true : IsIncludes<Rest, Sym> : false

type DropString<S extends string, R extends string, Res extends string = ''> =
  S extends `${infer First}${infer Rest}`
    ? IsIncludes<R, First> extends true
      ? DropString<Rest, R, Res>
      : DropString<Rest, R, `${Res}${First}`>
    : Res

//  2822 - Split
type Split<S extends string, SEP extends string, Res extends string[]=[]> = string extends S
  ? string[]
  : S extends SEP
    ? Res
    : S extends `${infer First}${SEP}${infer Rest}`
      ? Split<Rest, SEP, [ ...Res, First ]>
      : [ ...Res, S ]

//  2828 - ClassPublicKeys
type ClassPublicKeys<T> = keyof T

//  2857 - IsRequiredKey
type IsRequiredKey<T, K extends keyof T> = Pick<T, K> extends Required<Pick<T, K>> ? true : false

//  2949 - ObjectFromEntries
type ObjectFromEntries<T extends any[]> = {
  [Pair in T as Pair[0]] : Pair[1]
}

//  4037 - IsPalindrome
type IsPalindrome<T extends string | number, S extends string = `${T}`, Res extends string = ''>
  = S extends `${infer First}${infer Rest}` ? IsPalindrome<T, Rest, `${First}${Res}`>
  : `${T}` extends Res ? true : false

//  5181 - Mutable Keys
type MutableKeys<T> = keyof {
  [Prop in keyof T as Equal<Readonly<Pick<T, Prop>>, Pick<T, Prop>> extends true ? never : Prop] : never
}

//  9384 - Maximum
type Check2<T extends any[], N extends number, Res extends any[] = []>
  = T extends [infer First, ...infer Rest] ? First extends N ? Check2<Rest, N, Res> : Check2<Rest, N, [...Res, First]> : Res

//  19458 - SnakeCase
type SnakeCase<T extends string,Res extends string = ""> =
  T extends `${infer First}${infer Rest}` ?
    SnakeCase<Rest, First extends Uppercase<First> ? `${Res}_${Lowercase<First>}` : `${Res}${First}`>
    : Res

// 25747 - IsNegativeNumber
type IsUnion3<T, U = T> = T extends U ? [U] extends [T] ? false : true : false
type IsRealNumber<T> = T extends number ? number extends T ? false : true : false

type IsNegativeNumber<T extends number, V extends string = `${T}`>
  = IsRealNumber<T> extends true ? IsUnion3<T> extends true ? never : V extends `-${infer Rest}` ? true : false : never

// total 23
