// HARD
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

//  300 - String to Number
type Digits = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'

type IsNotValidNumber<T extends string> = T extends `${infer First}${infer Rest}` ? First extends Digits ? IsNotValidNumber<Rest> : true : false

type ToNumber<S extends string, Res extends any[] = []>
  = IsNotValidNumber<S> extends true ? never : `${Res['length']}` extends S ? Res['length'] : ToNumber<S, [...Res, 0]>

//  651 - Length of String 2
type LengthOfString2<S extends string, Res extends string[] = []> = S extends `${infer First}${infer Rest}` ? LengthOfString<Rest, [...Res, '0']> : Res['length']


// 847 - String Join
type Join<D extends string, T extends readonly string[], Res extends string = ''>
  = T extends [infer First extends string, ...infer Rest extends string[]] ? Join<D, Rest, `${Res}${Res extends '' ? '' : D}${First}`> : Res

type Result<D extends string, P extends readonly string[]> =
  P['length'] extends 0 ? ''
    : P['length'] extends 1 ? P[0]
      : Join<D, P>

declare function join<D extends string>(delimiter: D): <P extends readonly string[]>(...parts: P) => Result<D, P>

//  9384 - Maximum
type Check2<T extends any[], N extends number, Res extends any[] = []>
  = T extends [infer First, ...infer Rest] ? First extends N ? Check2<Rest, N, Res> : Check2<Rest, N, [...Res, First]> : Res

type Maximum<T extends any[], I extends any[] = []>
  = T extends [] ? never : Check2<T, I['length']>['length'] extends 1 ? Check2<T, I['length']>[0] : Maximum<Check2<T, I['length']>, [...I, 1]>

//  4037 - IsPalindrome
type IsPalindrome<T extends string | number, S extends string = `${T}`, Res extends string = ''>
  = S extends `${infer First}${infer Rest}` ? IsPalindrome<T, Rest, `${First}${Res}`>
  : `${T}` extends Res ? true : false

// 25747 - IsNegativeNumber
type IsUnion3<T, U = T> = T extends U ? [U] extends [T] ? false : true : false
type IsRealNumber<T> = T extends number ? number extends T ? false : true : false

type IsNegativeNumber<T extends number, V extends string = `${T}`>
  = IsRealNumber<T> extends true ? IsUnion3<T> extends true ? never : V extends `-${infer Rest}` ? true : false : never
