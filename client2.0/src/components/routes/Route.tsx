import classNames from 'classnames';
import React from 'react';
import './Route.scss';

type RouteProps = React.HTMLAttributes<HTMLDivElement>

const Route: React.FC<RouteProps> = ({ children, className, ...props }) => {
  return (
    <div className={classNames('route', className)} {...props}>
      {children}
    </div>
  );
};

export default Route;