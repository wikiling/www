import React from 'react';
import "./YScrollable.scss";

const YScrollable: React.FC = ({ children }) => (
  <div className="y-scrollable">
    {children}
  </div>
);

export default YScrollable