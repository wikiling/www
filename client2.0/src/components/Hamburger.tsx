import classNames from 'classnames';
import "./Hamburger.scss";
import React from 'react';

type HamburgerProps = React.HTMLAttributes<HTMLDivElement>

const Hamburger: React.FC<HamburgerProps> = ({ className = '', children, ...props }) => {
  return (
    <div className={classNames("hamburger", "hamburger--squeeze", className)} {...props}>
      <div className="hamburger-box">
        <div className="hamburger-inner"></div>
      </div>
    </div>
  )
};

export default Hamburger