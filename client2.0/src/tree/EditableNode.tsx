import { useForm } from "react-hook-form";
import { TreeNode } from "./types";

type EditableNodeProps = {
  node: TreeNode
}
  
const EditableNode: React.FC<EditableNodeProps> = ({ node }) => {
  const {
    register,
    handleSubmit,
    formState: { errors },
  } = useForm()

  return (
    <foreignObject x={node.x} y={node.y} className="node node--editable">
      <form>
        <input {...register("text")}></input>
      </form>
    </foreignObject>
  );
};

export default EditableNode