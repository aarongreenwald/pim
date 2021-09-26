import * as React from 'react';
import {Link} from 'react-router-dom';

export const Breadcrumbs = ({breadcrumbs}) => (
    <h3>
        {breadcrumbs.map((breadcrumb) =>
            <span key={breadcrumb.name}><Link to={`/notes/?path=${encodeURIComponent(breadcrumb.path)}`}>{breadcrumb.name}</Link> /</span>
        )}
    </h3>
);