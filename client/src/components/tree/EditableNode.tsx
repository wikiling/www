import Field from 'components/forms/Field';
import { forwardRef, useEffect, useRef, useState } from 'react'
import { useForm, UseFormSetFocus } from "react-hook-form";
import { getTextDimensions } from 'utils/document';
import { EditableNodeValues, CoordinatedTreeNode } from "./types";
import { nodeText } from './utils';

type EditableNodeProps = {
  node: CoordinatedTreeNode
  onSubmit: (values: EditableNodeValues) => void
}

const EditableNode = forwardRef<
  SVGGElement, EditableNodeProps
>(({ node, onSubmit }, forwardedRef) => {
  const fieldRef = useRef<HTMLInputElement | null>(null);
  const fieldName = 'text';
  const initialValue = nodeText(node);
  const initialValueDims = getTextDimensions(initialValue);
  const {
    register,
    handleSubmit,
    setFocus
  } = useForm<EditableNodeValues>({
    defaultValues: {
      [fieldName]: initialValue,
      id: node.data.id
    }
  });

  const [width, setWidth] = useState<number>(1.25 * initialValueDims.width);
  const [height, setHeight] = useState<number>(1.25 * initialValueDims.height);
  const [x, setX] = useState<number>(node.x - width / 2);
  const [y, setY] = useState<number>(node.y - initialValueDims.height / 2);

  const handleChange = () => {
    if (!fieldRef.current) return

    const w = fieldRef.current.offsetWidth,
          h = fieldRef.current.offsetHeight

    setWidth(w);
    setHeight(h)

    setX(node.x - w / 2);
    setY(node.y - h / 2);
  };

  const {
    ref: formRef,
    ...registration
  } = register(fieldName, { onChange: handleChange });

  const assignFieldRef = (el: HTMLInputElement | null) => {
    formRef(el);
    fieldRef.current = el;
  };

  useEffect(() => {
    setFocus('text');
  }, [])

  return (
    <g ref={forwardedRef} className="node node-editable">
      <rect x={x} y={y} width={width} height={height} fill="white"/>
      <foreignObject
        width={width}
        height={height}
        x={x}
        y={y}
      >
        <form onSubmit={handleSubmit(onSubmit)}>
          <Field
            initialValue={initialValue}
            ref={assignFieldRef}
            {...registration}
          />
        </form>
      </foreignObject>
    </g>
  );
});

export default EditableNode