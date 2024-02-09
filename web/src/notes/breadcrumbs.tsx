import * as React from 'react';
import {Link} from 'react-router-dom';

interface BreadcrumbsProps {
  breadcrumbs: {
    name: string;
    path: string;
  }[];
}
export const Breadcrumbs: React.FC<BreadcrumbsProps> = ({breadcrumbs}) => (
  <h3>
    {breadcrumbs.map((breadcrumb, i) =>
      <span key={breadcrumb.name}>
        <Link to={`/notes/?path=${encodeURIComponent(breadcrumb.path)}`}>{breadcrumb.name}</Link>
        {
          i !== breadcrumbs.length - 1 && ' /'
        }
      </span>
    )}
  </h3>
);
