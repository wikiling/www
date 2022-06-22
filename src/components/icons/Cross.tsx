import React, { SVGProps } from 'react';

type Props = SVGProps<SVGSVGElement>

const Cross: React.FC<Props> = ({ height = 15, width = 15, ...props }) => (
  <svg
    width={width}
    height={height}
    viewBox="0 0 265.1 326.15"
    {...props}
  >

  <g
      id="layer1"
      transform="translate(-225.26 -413.57)"
    >
    <g
        id="g22143"
        transform="matrix(10.297 0 0 10.297 -6578.9 -4137.6)"
      >
      <g
          id="g22133"
          fill="#d40000"
          transform="matrix(.45326 0 0 .45326 644.4 113.53)"
        >
        <path
            id="path22135"
            d="m36.097 739.31 9.6926-14.678c16.511 12.907 17.767 19.639 24.949 30.909 12.01 18.507 16.731 24.858 22.153 25.922-6.5016 4.5987-8.5708 6.3034-13.275 13.046-16.193-26.29-27.333-53.62-43.523-55.2z"
            fill="#d40000"
        />
      </g
      >
      <g
          id="g22139"
          fill="#d40000"
          transform="matrix(-.45326 0 0 .45326 702.87 113.53)"
        >
        <path
            id="path22141"
            fill="#d40000"
            d="m36.097 739.31 9.6926-14.678c16.511 12.907 17.767 19.639 24.949 30.909 12.01 18.507 16.731 24.858 22.153 25.922-6.5016 4.5987-8.5708 6.3034-13.275 13.046-16.193-26.29-27.333-53.62-43.523-55.2z"
        />
      </g>
    </g>
  </g>
  </svg>
)

export default Cross;