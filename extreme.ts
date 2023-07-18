import type { Equal } from '@type-challenges/utils'

//  5 - Get Readonly Keys
type GetReadonlyKeys<T> = keyof {
  [Prop in  keyof T as Equal<Readonly<Pick<T, Prop>>, Pick<T, Prop>> extends true ? Prop : never]: T[Prop]
}
