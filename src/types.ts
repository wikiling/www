import { HierarchyNode } from "d3-hierarchy"

export type toEnumType<EnumType> = EnumType[keyof EnumType]

export type ID = number
export type UUID = string
export type Slug = string

export type Author = {
  id: ID
  full_name: string
}

export type TreeID = string;

export type SyntaxTree = {
  id: TreeID
  label: string
  children?: SyntaxTree[]
}

export type SemanticTree = {
  id: TreeID
  expr: string
  type: string
  typeError: string
  value: string
  valuationError: string
  syntaxLabel: string
  children?: [SemanticTree]
}

export type CoordinatedSyntaxTree = HierarchyNode<SyntaxTree>
export type CoordinatedSemanticTree = HierarchyNode<SemanticTree>

export type ExampleBase = {
  fragment_id: ID
  description: string
  content: string
  label: string
}

export type Example = ExampleBase & { id: ID }
export type TemporaryExample = ExampleBase & {
  temp_id: UUID
}

export type ExampleEditValues = Pick<ExampleBase, 'label' | 'content'>
export type ExampleCreateValues = ExampleBase

export type InterpretationBase = {
  example_id: ID
  content: string
  paraphrase: string
}

export type Interpretation = InterpretationBase & {
  id: ID
  constituency_parse?: ConstituencyParse
}

export type TemporaryInterpretation = InterpretationBase & {
  temp_id: UUID
}

export type InterpretationEditValues = Pick<InterpretationBase, 'content'>
export type InterpretationCreateValues = InterpretationBase

export type ConstituencyParse = {
  id: ID
  interpretation_id: ID
  parse_string: string
  syntax_tree: SyntaxTree
}

export type ConstituencyParseNodeEditValues = {
  id: TreeID
  label: string
}

export type ConstituencyParseEditValues = {
  parse_string: string
}

export type CoordinatedConstituencyParse = ConstituencyParse & {
  coordinated_syntax_tree: CoordinatedSyntaxTree
}

export type Fragment = {
  id: ID
  slug: Slug
  author: Author
  title: string
  examples: Example[]
}

export type FragmentDetailRouteParams = {
  fragmentSlug: Slug
}