import React, { useRef } from 'react';
import * as ReactDOMServer from 'react-dom/server';
import ContentEditable, { ContentEditableEvent } from 'react-contenteditable';
import Bracketing, { BracketingProps } from './Bracketing';

type EditableBracketingProps = BracketingProps & {
  onClick: React.MouseEventHandler<HTMLDivElement>
}

const parser = new DOMParser();

const EditableBracketing: React.FC<EditableBracketingProps> = ({ tree, onClick }) => {
  const ref = useRef<HTMLElement>();
  const html = useRef(
    ReactDOMServer.renderToString(<Bracketing tree={tree}/>)
  );

  const handleChange = (evt: ContentEditableEvent) => {
    html.current = evt.target.value;
    console.log(html.current);
    const dom = parser.parseFromString(html.current, "text/html");
    console.log(dom)
    console.log(dom.children)
  };

  return (
    <ContentEditable
      // innerRef={ref}
      onClick={onClick}
      html={html.current} // innerHTML of the editable div
      disabled={false}
      onChange={handleChange} // handle innerHTML change
      tagName='span' // Use a custom HTML tag (uses a div by default)
    />
  );
};

export default EditableBracketing;