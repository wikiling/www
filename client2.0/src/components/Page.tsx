import React from 'react';

const Page: React.FC = ({ children }) => {
  return (
    <div className="page">
      {children}
    </div>
  );
};

export default Page