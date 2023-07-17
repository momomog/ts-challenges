// MEDIUM
// 2 - Get Return Type
type MyReturnType<T> = T extends (...args: any[]) => infer R ? R : never

// 3 - Omit
type MyOmit<T, K extends keyof T> = {
  [Property in keyof T as Property extends K ? never : Property]: T[Property]
}

//  8 - Readonly 2
type MyReadonly2<T, K extends keyof T = keyof T> = Omit<T, K> & Readonly<Pick<T, K>>

// 9 - Deep Readonly
type DeepReadonly<T> = {
  readonly [Property in keyof T]: T[Property] extends Function ? T[Property] : T[Property] extends object ? DeepReadonly<T[Property]> : T[Property]
}

//  10 - Tuple to Union
type TupleToUnion<T extends any[]> = T[number]

//  12 - Chainable Options
type Chainable<T extends {}> = {
  option<K extends string, V>(key: K, value: V): Chainable<keyof T extends K ? Omit<T, K> & Record<K, V> : T & Record<K, V>>
  get(): T
}

//  15 - Last of Array
type Last<T extends any[]> = [never, ...T][T['length']]

// 16 - Pop
type Pop<T extends any[]> = T extends [] ? [] : T extends [...infer R, infer _] ? R : never

// 20 - Promise.all
declare function PromiseAll<T extends any[]>(values: readonly [...T]): Promise<{
  [Prop in keyof T]: Awaited<T[Prop]>
}>

//  62 - Type Lookup
type LookUp<U, T> = U extends { type: T } ? U : never

//  106 - Trim Left
type Empty1 = ' ' | '\n' | '\t'
type TrimLeft<S extends string> = S extends `${Empty1}${infer Rest}` ? TrimLeft<Rest> : S

//  108 - Trim
type Empty2 = ' ' | '\n' | '\t'
type Trim<S extends string> = S extends `${Empty2}${infer Rest}` ? Trim<Rest> : S extends `${infer Rest}${Empty2}` ? Trim<Rest> : S

// 110 - Capitalize
type MyCapitalize<S extends string> = S extends `${infer First}${infer Rest}` ? `${Uppercase<First>}${Rest}` : ''

//  116 - Replace
type Replace<S extends string, From extends string, To extends string> =
  From extends '' ? S : S extends `${infer Start}${From}${infer End}` ? `${Start}${To}${End}` : S

//  119 - ReplaceAll
type ReplaceAll<S extends string, From extends string, To extends string> =
  From extends '' ? S :
    S extends `${infer Start}${From}${infer End}` ? `${ReplaceAll<Start, From, To>}${To}${ReplaceAll<End, From, To>}` : S

//  191 - Append Argument
type AppendArgument<Fn, A> = Fn extends (...args: infer Args) => infer ReturnType ? (...args: [...Args, A]) => ReturnType : never

// Permutation
type Permutation<T, K=T> = [T] extends [never] ? [] : K extends K ? [K, ...Permutation<Exclude<T, K>>] : never

//  298 - Length of String
type LengthOfString<S extends string, Length extends string[] = []> = S extends `${infer firstChar}${infer rest}` ? LengthOfString<rest, [...Length, firstChar]> : Length['length']

// 459 - Flatten
type Flatten<T extends any[], Result extends any[] = []> =
  T extends [infer FirstEl, ...infer Rest]
    ? FirstEl extends  any[] ? Flatten<[...FirstEl, ...Rest], Result> : Flatten<Rest, [...Result, FirstEl]> : Result

// 527 - Append to object
type AppendToObject<T, U extends keyof any, V> = {
  [Prop in keyof T | U]: Prop extends keyof T ? T[Prop] : V
}

// 529 - Absolute
type Absolute<T extends number | string | bigint> = `${T}` extends `${infer FirstChar}${infer Rest}` ? FirstChar extends '-' ? Rest : `${T}` : `${T}`

//  531 - String to Union
type StringToUnion<T extends string, Res extends string[] = []> =
  T extends ' ' ? never : T extends `${infer FirstChar}${infer Rest}` ? StringToUnion<Rest, [...Res, FirstChar]> : Res[number]

//  599 - Merge
type Merge<F extends {}, S extends {}> = {
  [Prop in keyof S | keyof F]: Prop extends keyof F & keyof S ? S[Prop] : Prop extends keyof F ? F[Prop] : Prop extends keyof S ? S[Prop] : never
}

//  612 - KebabCase
type SmileSymbols = '😎'
type IgnoredSymbols = '-' | '_'

type KebabCase<S extends string, Result extends string = ''> =
  S extends SmileSymbols ? S : S extends `${infer First}${infer Rest}`
    ? First extends `${Uppercase<First>}`
      ? First extends IgnoredSymbols
        ? KebabCase<`${Rest}`, `${Result}${First}`>
        : KebabCase<`${Rest}`, `${Result}${Result extends '' ? '' : '-'}${Lowercase<First>}`>
      : KebabCase<Rest, `${Result}${First}`>
    : Result

//  645 - Diff
type Diff<T1, T2> = {
  [Prop in keyof T1 | keyof T2 as Prop extends keyof T1 & keyof T2 ? never : Prop]: Prop extends keyof T1 ? T1[Prop] : Prop extends keyof T2 ? T2[Prop] : never
}

//  949 - AnyOf
type AnyOf<T extends readonly any[]> = T[number] extends 0 | '' | false | [] | {[key: string]: never} | null | undefined
  ? false
  : true

//  1042 - IsNever
type IsNever<T> = [T] extends [never] ? true : false

//  1097 - IsUnion
type Is<T, K=T> = [T] extends [never] ? [] : K extends K ? [K, ...Is<Exclude<T, K>>] : never
type IsUnion<T> = Is<T>['length'] extends 0 | 1 ? false : true

//  1130 - ReplaceKeys
type ReplaceKeys<U, T, Y> = {
  [Prop in keyof U]: Prop extends T ? Prop extends keyof Y ? Y[Prop] : never : U[Prop]
}

//  1367 - Remove Index Signature
type FilterKey<Key> = string extends Key ? never : number extends Key ? never : symbol extends Key ? never : Key
type RemoveIndexSignature<T> = {
  [Key in keyof T as FilterKey<Key>]: T[Key]
}

//  1978 - Percentage Parser
type Suffix<T> = T extends '-' | '+' ? T : never
type Postfix<T> = T extends `${infer Middle}%` ? [Middle, '%'] : [T, '']
type PercentageParser<A extends string> = A extends `${Suffix<infer First>}${infer Last}` ?  [First, ...Postfix<Last>] : ['', ...Postfix<A>]

//  2070 - Drop Char
type DropChar<S, C extends string, Result extends string = ''> = S extends `${infer Char}${infer Rest}` ? DropChar<Rest, C, `${Result}${Char extends C ? '' : Char}`> : Result

//  2257 - MinusOne
type MinusOne<T extends number, Res extends number[] = []> = T extends 0 ? -1 : Res['length'] extends T ? Res extends [infer First, ...infer Rest] ? Rest['length'] : never : MinusOne<T, [...Res, 0]>

//  2595 - PickByType
type Check<T, Prop extends keyof T, U> = T[Prop] extends U ? Prop : never

type PickByType<T, U> = {
  [Prop in keyof T as Check<T, Prop, U>]: T[Prop]
}

//  2688 - StartsWith
type StartsWith<T extends string, U extends string> = U extends T | '' ? true : T extends `${U}${string}` ? true : false

//  2693 - EndsWith
type EndsWith<T extends string, U extends string> =  U extends T | '' ? true : T extends `${string}${U}` ? true : false

// 2757 - PartialByKeys
type PartialByKeys1Var<T, K extends keyof T> = Omit<T, K> & Partial<{ [Prop in K]: T[Prop] }>

type PartialByKeys2Var<T, K extends keyof T> = {
  [Prop in keyof T]: Prop extends K ? T[Prop] | undefined : T[Prop]
}

// 2759 - RequiredByKeys
type RequiredByKeys<T, K extends keyof T> = Required<Pick<T, K>> & Omit<T, K>

//  2793 - Mutable
type Mutable<T> = {
  -readonly [Prop in keyof T]: T[Prop]
}

//  2852 - OmitByType
type IsValid<T, Prop extends keyof T, U> = T[Prop] extends U ? never : Prop

type OmitByType<T, U> = {
  [Prop in keyof T as IsValid<T, Prop, U>]: T[Prop]
}

// 2946 - ObjectEntries
type ObjectEntries<T, Res = Required<T>> = {
  [Prop in keyof Res]: [Prop, [Res[Prop]] extends [never] ? undefined : Res[Prop]]
}[keyof Res]

// 3062 - Shift
type Shift<T extends any[]> = T extends [infer First, ...infer Rest] ? Rest : []

//  3188 - Tuple to Nested Object
type TupleToNestedObject<T extends any[], U> = [T] extends [[]] | [never] ? U : {
  [Key in T[0]]: T extends [infer First, ...infer Rest] ? Rest extends [] ? U : TupleToNestedObject<Rest, U> : never
}

//  3192 - Reverse
type Reverse<T extends unknown[], Res extends unknown[] = []> = T extends [...infer First, infer Last] ? Reverse<First, [...Res, Last]> : Res

//  3196 - Flip Arguments
type ReverseArray<T extends any[]> = T extends [infer First, ...infer Rest] ? [...ReverseArray<Rest>, First] : []

type FlipArguments<T extends (...args: any[]) => any> = T extends (...args: infer Args) => infer Result ? (...args: ReverseArray<Args>) => Result : never

//  3243 - FlattenDepth
type FlattenDepth<T extends any[], C extends number = 1, Tmp extends any[] = []> = T extends [infer First,...infer Rest]?
  First extends any[] ?
    Tmp['length'] extends C ?
      [First, ...FlattenDepth<Rest, C, Tmp>]
      :[...FlattenDepth<First, C, [0,...Tmp]>,...FlattenDepth<Rest, C, Tmp>]
    :[First,...FlattenDepth<Rest, C, Tmp>]
  : T

//  3326 - BEM style string
type BEM<B extends string, E extends string[], M extends string[]> = `${B}${E extends [] ? '' : `__${E[number]}`}${M extends [] ? '' : `--${M[number]}`}`

//  3376 - InorderTraversal
interface TreeNode {
  val: number
  left: TreeNode | null
  right: TreeNode | null
}
type InorderTraversal<T extends TreeNode | null, NT extends TreeNode = NonNullable<T>> = T extends null
  ? []
  : [...InorderTraversal<NT['left']>, NT['val'], ...InorderTraversal<NT['right']>]

//  4179 - Flip
type Flip2<T> = {
  [Prop in keyof T as T[Prop] extends boolean ? T[Prop] extends true ? 'true' : 'false' : (number | string) & T[Prop]]: Prop
}

//  4260 - AllCombinations
type String2Union<S extends string> =
  S extends `${infer C}${infer REST}`
    ? C | String2Union<REST>
    : never;

type AllCombinations<
  STR extends string,
  S extends string = String2Union<STR>,
> = [S] extends [never]
  ? ''
  : '' | {[K in S]: `${K}${AllCombinations<never, Exclude<S, K>>}`}[S];

// 4425 - Greater Than
type GreaterThan<T extends number, U extends number, TArr extends number[] = [], UArr extends number[] = []>
  = TArr['length'] extends T ? false : UArr['length'] extends U ? true : GreaterThan<T, U, [...TArr, 1], [...UArr, 1]>

//  4484 - IsTuple
type IsTuple<T> = [T] extends [never] ? false : T extends readonly any[] ? number extends T['length'] ? false : true : false

//  4803 - Trim Right
type Empty = ' ' | '\n' | '\t'
type TrimRight<S extends string> = S extends `${infer Start}${Empty}` ? TrimRight<Start> : S

//  5117 - Without
type Check1<T extends unknown, U extends number | number[]> = U extends number[] ? T extends U[number] ? true : false : T extends U ? true : false

type Without<T extends unknown[], U extends number | number[], Res extends unknown[] = []>
  = T extends [infer First, ...infer Rest] ? Without<Rest, U, Check1<First, U> extends true ? Res : [...Res, First] > : Res

//  5140 - Trunc
type Trunc<T extends string | number, Res extends string = '', IsFirstIteration extends boolean = true>
  = `${T}` extends `${infer First}${infer Rest}` ? First extends '.' ? IsFirstIteration extends true ? '0' : Res : Trunc<Rest, `${Res}${First}`, false> : Res

//  5153 - IndexOf
type IsEqual<T, U> = T extends U ? U extends T ? true : false : false

type IndexOf<T extends any[], U, Res extends any[] = []> =
  T extends [infer First, ...infer Rest] ? IsEqual<First, U> extends true ? Res['length'] : IndexOf<Rest, U, [...Res, First]> : -1

//  5310 - Join
type Join<T extends unknown[], U extends string | number, Res extends string = ''>
  = T extends [infer First extends string, ...infer Rest] ? Join<Rest, U, `${Res}${First}${Rest extends [] ? '' : U}`> : Res

//  5317 - LastIndexOf
type IsEqual1<T, U> = T extends U ? U extends T ? true : false : false

type LastIndexOf<T extends any[], U, ResArr extends any[] = [], Index extends number = -1>
  = T extends [infer First, ...infer Rest] ? LastIndexOf<Rest, U, [...ResArr, First], IsEqual1<U, First> extends true ? ResArr['length'] : Index> : Index

//  5360 - Unique
type Includes1<T, U extends any[]> = U extends [infer First, ...infer Rest] ? Equal<T, First> extends true ? true : Includes1<T, Rest> : false

type Unique<T extends any[], Res extends any[] = []> = T extends [infer First, ...infer Rest] ? Unique<Rest, Includes1<First, Res> extends true ? Res : [...Res, First]> : Res

//  5821 - MapTypes
type Map1 = { mapFrom: any, mapTo: any }
type MapTypes<T extends object, R extends Map1> = {
  [Prop in keyof T]: T[Prop] extends R['mapFrom'] ? R extends { mapFrom: T[Prop] } ? R['mapTo'] : never : T[Prop]
}

//  7544 - Construct Tuple
type ConstructTuple<L extends number, Res extends any[]= []> = Res['length'] extends L ? Res : ConstructTuple<L, [...Res, unknown]>

//  8640 - Number Range
type FilterRange<T extends number, Arr extends number[], Res extends number[] = []>
  = Arr extends [infer First extends number, ...infer Rest extends number[]] ? FilterRange<T, Rest, First extends T ? [First] : [...Res, First]> : Res[number]

type NumberRange<L extends number, H extends number, Res extends number[] = []>
  = Res['length'] extends H ? FilterRange<L, [...Res, H]> : NumberRange<L, H, [...Res, Res['length']]>

//  8987 - Subsequence
type Subsequence<T> = T extends [infer First, ...infer Rest]
  ? [First] | [...Subsequence<Rest>] | [First, ...Subsequence<Rest>]
  : []

//  9142 - CheckRepeatedChars
type CheckRepeatedChars<T extends string, Res extends string[] = []>
  = T extends `${infer First}${infer Rest}` ? First extends Res[number] ? true : CheckRepeatedChars<Rest, [...Res, First]> : false

//  9286 - FirstUniqueCharIndex
type FirstUniqueCharIndex<T extends string, Res extends string[] = []> = T extends '' ? -1 :
  T extends `${infer First}${infer Rest}`
    ? First extends Res[number]
      ? FirstUniqueCharIndex<Rest, [...Res, First]>
      : Rest extends `${any}${First}${any}` ? FirstUniqueCharIndex<Rest, [...Res, First]> : Res['length']
    : never

// 9896 - GetMiddleElement
type GetMiddleElement<T extends any[]> = T extends [infer U, ...infer T, infer Y] ? T extends [] ? [U, Y] : GetMiddleElement<T> : T

//  9898 - Appear only once
type FindEles<T extends any[], Past extends any[] = [],Res extends any[] = []>
  = T extends [infer First, ...infer Rest] ? FindEles<Rest, [...Past, First], First extends Rest[number] | Past[number] ? [...Res] : [...Res, First]> : Res

//  10969 - Integer
type Integer<T extends number> = `${T}` extends `${infer Dig}.${any}` ? Dig extends number ? Dig : never : T extends number ? number extends T ? never : T : never

//  16259 - ToPrimitive
type IsPrimitive<T> = T extends number ? number : T extends string ? string : T extends boolean ? boolean : never

type ToPrimitive<T> = {
  [Prop in keyof T] : T[Prop] extends Function ? Function : T[Prop] extends object ? ToPrimitive<T[Prop]> : IsPrimitive<T[Prop]>
}

//  17973 - DeepMutable
type DeepMutable<T extends object> = {
  -readonly [Prop in keyof T]: T[Prop] extends Function ? T[Prop] : T[Prop] extends object ? DeepMutable<T[Prop]> : T[Prop]
}

//  18142 - All
type All<T extends any[], U, Res extends any[] = []> = T extends [infer First, ...infer Rest] ? Equal<First, U> extends false ? false : All<Rest, U> : true

//  18220 - Filter
type Filter<T extends any[], U, Res extends any[] = []> = T extends [infer First, ...infer Rest] ? Filter<Rest, U, First extends U ? [...Res, First] : Res> : Res

//  21106 - Combination key type
type Combinations<T extends string, Arr extends any[], Res extends any[] = []> = Arr extends [infer First extends string, ...infer Rest] ? Combinations<T, Rest, [...Res, `${T} ${First}`]> : Res
type Combs<T extends any[], Res extends any[] = []> = T extends [infer First extends string, ...infer Rest] ? Combs<Rest, [...Res, ...Combinations<First, Rest>]> : Res[number]

// 25170 - Replace First
type AreSomeEqual<T extends readonly any[], V> = T extends [infer First, ...infer Rest] ? First extends V ? true : AreSomeEqual<Rest, V> : false

type ReplaceFirst<T extends readonly unknown[], S, R, Res extends any[] = []> =
  AreSomeEqual<T, S> extends true
    ? T extends [infer First, ...infer Rest] ? First extends S ? [...Res, R, ...Rest] : ReplaceFirst<Rest, S, R, [...Res, First]> : Res
    : T

// 26401 - JSON Schema to TypeScript
type Json = { type: any, enum?: any[], properties?: Record<string, any>, items?: { type: string, properties?: Record<string, { type: any }> }, required?: string[] }
type Primitives = 'string' | 'number' | 'boolean'

type GetPrimitive<T extends Primitives> = T extends 'string' ? string : T extends 'number' ? number : boolean
type GetObject<T extends Json> =
  T['properties'] extends object ? T['required'] extends any[]
      ? { [Prop in keyof T['properties'] as Prop extends T['required'][number] ? never : Prop ]?: JSONSchema2TS<T['properties'][Prop]> } & { [Prop in keyof T['properties'] as Prop extends T['required'][number] ? Prop : never]: JSONSchema2TS<T['properties'][Prop]> }
      : { [Prop in keyof T['properties']]?: JSONSchema2TS<T['properties'][Prop]> }
    : Record<string, unknown>

type GetArray<T extends Json> =
  T['items'] extends object ? T['items']['type'] extends Primitives ? GetPrimitive<T['items']['type']>[] : T['items']['type'] extends 'object' ? GetObject<T['items']>[] : Record<string, unknown>[] : unknown[]

type JSONSchema2TS<T extends Json, Type = T['type']> =
  Type extends 'array' ? GetArray<T>
    : Type extends 'object' ? GetObject<T>
      : T['enum'] extends any[] ? T['enum'][number]
        : Type extends Primitives ? GetPrimitive<Type>
          : any

//  27133 - Square
type TotalLength<T extends any[][], Res extends any[] = []> =
  T extends [infer First extends any[], ...infer Rest extends any[][]] ? TotalLength<Rest, [...Res, ...First]> : Res['length']

type Normalize<T extends number> = `${T}` extends `-${infer Dig}` ? Dig : `${T}`

type Square<N extends number, Res extends any[] = [], TotalRes extends any[][] = [], NormalizeN = Normalize<N>> =
  `${Res['length']}` extends NormalizeN
    ? `${TotalRes['length']}` extends NormalizeN ? TotalLength<TotalRes> : Square<N, [], [...TotalRes, Res]>
    : Square<N, [...Res, 0], TotalRes>

//  27152 - Triangular number
type Filled<N extends number, Res extends number[] = []> = Res['length'] extends N ? Res : Filled<N, [...Res, 1]>

type Triangular<N extends number, It extends number[] = [], Res extends any[] = []>
  = It['length'] extends N ? [...Res, ...Filled<N>]['length'] : Triangular<N, [...It, 1], [...Res, ...Filled<It['length']>]>

//   27862 - CartesianProduct
type Sec<U> = U extends U ? [U] : never
type CartesianProduct<T, U> = T extends T ? [T, ...Sec<U>] : never

// total 78
