import React, { SVGProps } from 'react';

type Props = SVGProps<SVGSVGElement>

const Plus: React.FC<Props> = ({ height = 15, width = 15, fill = "grey", ...props}) => (
  <svg x="0px" y="0px" viewBox="0 0 512 512" width={width} height={height} fill={fill} {...props}>
    <polygon points="289.391,222.609 289.391,0 222.609,0 222.609,222.609 0,222.609 0,289.391 222.609,289.391 222.609,512 289.391,512 289.391,289.391 512,289.391 512,222.609"/>
  </svg>
)

export default Plus;