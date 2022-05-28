import React from 'react';
import "./Page.scss";
import { SplitPane } from "react-collapse-pane";

type PageProps = {
  header: React.ReactNode
  panes: React.ReactNode[]
}

const Page: React.FC<PageProps> = ({ header, panes }) => {
  return (
    <div className="page">
      {header}
      <div className="page-body">
        <SplitPane
          split="vertical"
          resizerOptions={{
            css: {
              width: '1px',
              background: 'rgba(0, 0, 0, 0.1)',
            },
            hoverCss: {
              width: '3px',
              background: '1px solid rgba(102, 194, 255, 0.5)',
            },
            grabberSize: '1rem',
          }}>
          {panes.map((pane, idx) => <>{pane}</>)}
        </SplitPane>
      </div>
    </div>
  );
};

export default Page