import classNames from 'classnames';
import "./Drawer.scss";
import React, { useState } from 'react';
import Hamburger from './Hamburger';

type DrawerProps = React.HTMLAttributes<HTMLDivElement>

const Drawer: React.FC<DrawerProps> = ({ className = '', children, ...props }) => {
  const [active, setActive] = useState<boolean>(false);

  return (
    <div className={classNames("drawer", className)} {...props}>
      <div className="drawer-icon">
        <Hamburger/>
      </div>
      <div className="drawer-contents">

      </div>
    </div>
  )
};

export default Drawer