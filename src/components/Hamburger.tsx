import classNames from 'classnames';
import "./Hamburger.scss";
import React, { useState } from 'react';

type HamburgerProps = React.HTMLAttributes<HTMLDivElement>

const Hamburger: React.FC<HamburgerProps> = ({ className = '', children, ...props }) => {
  const [active, setActive] = useState<boolean>(false);
  return (
    <div
      onClick={() => setActive(!active)}
      className={classNames(
        "hamburger",
        "hamburger--squeeze",
        { active },
        className
      )}
      {...props}>
      <div className="hamburger-box">
        <div className="hamburger-inner"></div>
      </div>
    </div>
  )
};

export default Hamburger