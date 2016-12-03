
import React from 'react'
import { render } from 'react-dom'
import { Router, Route, browserHistory } from 'react-router'
import App from './components/app/index'

render((
    <Router history={browserHistory}>
        <Route component={App} path="/" />
        <Route component={App} path="/entries" />
    </Router>
), document.getElementById('app'))


/*
    "indent": ["error", 2],
    "linebreak-style": ["error", "unix"],
    "quotes": ["error", "single"],
    "semi": ["error", "never"],

    "react/display-name": 0,
    "react/forbid-prop-types": 1,
    "react/no-danger": 1,
    "react/no-deprecated": 1,
    "react/no-did-mount-set-state": 1,
    "react/no-did-update-set-state": 1,
    "react/no-direct-mutation-state": 1,
    "react/no-is-mounted": 1,
    "react/no-multi-comp": 1,
    "react/no-set-state": 1,
    "react/no-string-refs": 1,
    "react/no-unknown-property": 1,
    "react/prefer-es6-class": 1,
    "react/prefer-stateless-function": 0,
    "react/prop-types": 1,
    "react/react-in-jsx-scope": 1,
    "react/require-extension": 1,
    "react/require-render-return": 1,
    "react/self-closing-comp": 1,
    "react/sort-comp": 1,
    "react/sort-prop-types": 1,
    "react/wrap-multilines": 1,

    "react/jsx-boolean-value": 1,
    "react/jsx-closing-bracket-location": 1,
    "react/jsx-curly-spacing": 1,
    "react/jsx-equals-spacing": 1,
    "react/jsx-first-prop-new-line": 1,
    "react/jsx-handler-names": 1,
    "react/jsx-indent-props": 1,
    "react/jsx-indent": 1,
    "react/jsx-key": 1,
    "react/jsx-max-props-per-line": 0,
    "react/jsx-no-bind": 1,
    "react/jsx-no-duplicate-props": 1,
    "react/jsx-no-literals": 1,
    "react/jsx-no-target-blank": 1,
    "react/jsx-no-undef": 1,
    "react/jsx-pascal-case": 1,
    "react/jsx-sort-props": 1,
    "react/jsx-space-before-closing": 1,
 */