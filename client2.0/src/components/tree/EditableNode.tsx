import { forwardRef } from 'react'
import { useForm } from "react-hook-form";
import { EditableNodeValues, CoordinatedTreeNode } from "./types";
import { nodeText } from './utils';

type EditableNodeProps = {
  node: CoordinatedTreeNode
  onSubmit: (values: EditableNodeValues) => void
}

const EditableNode = forwardRef<
  HTMLFormElement, EditableNodeProps
>(({ node, onSubmit }, ref) => {
  const fieldName = "text"
  const {
    register,
    handleSubmit,
  } = useForm({
    defaultValues: {
      [fieldName]: nodeText(node),
      id: node.data.id
    }
  })

  const width = 152;
  const height = 50;
  const x = node.x - width / 2, y = node.y;

  return (
    <foreignObject height={height} width={width} x={x} y={y} className="node node--editable">
      <form onSubmit={handleSubmit(onSubmit)} ref={ref}>
        <input {...register(fieldName)}></input>
      </form>
    </foreignObject>
  );
});

export default EditableNode