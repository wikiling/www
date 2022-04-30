import classNames from 'classnames';
import "./Button.scss";
import React from 'react';

type ButtonProps = React.HTMLAttributes<HTMLDivElement> & {
  trans?: boolean
  active?: boolean
}

const Button: React.FC<ButtonProps> = ({ className = '', trans = true, active = false, children, ...props }) => {
  return (
    <div className={classNames(
      'button',
      className,
      {
        'button--trans': trans,
        'button--active': active
      }
    )} {...props}>
      {children}
    </div>
  )
};

export default Button