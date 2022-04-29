import classNames from 'classnames';
import "./Button.scss";
import React from 'react';

type ButtonProps = React.HTMLAttributes<HTMLDivElement> & {
  trans?: boolean
}

const Button: React.FC<ButtonProps> = ({ className = '', trans = false, children, ...props }) => {
  return (
    <div className={classNames('button', className, { 'button--trans': trans })} {...props}>
      {children}
    </div>
  )
};

export default Button