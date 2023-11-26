import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
export const _SvgProgress = ({ progress, svg }) => {
    const id = 'svg-filter';
    const scale = progress / 100;
    return (_jsx("div", { style: {
            filter: `url(#${id})`,
            transform: `scaleY(${scale})`,
            transformOrigin: 'bottom',
        }, children: _jsxs("svg", { width: '160', height: '42', viewBox: '0 0 160 42', fill: 'none', xmlns: 'http://www.w3.org/2000/svg', "data-logo": 'true', children: [_jsx("defs", { children: _jsx("filter", { id: id, children: _jsx("feColorMatrix", { type: 'matrix', values: '.33 .33 .33 0 0 .33 .33 .33 0 0 .33 .33 .33 0 0 0 0 0 1 0' }) }) }), _jsxs("g", { "clip-path": "url(#logo__a)", children: [_jsx("path", { d: "M21.217 1.748 18.185 0 0 10.5v21l.699.403 2.333 1.345L18.185 42l6.064-3.5v-21l-12.125-7-3.032 1.748 12.125 7v17.5L18.185 38.5 3.032 29.748v-17.5L18.185 3.5l15.156 8.748v21l3.032-1.748v-21L21.217 1.748Z", fill: "#511CF7" }), _jsx("path", { d: "m15.156 8.748 12.121 7v21L30.31 35V14L18.185 7l-3.029 1.748Z", fill: "#511CF7" })] }), _jsx("defs", { children: _jsx("clipPath", { id: "logo__a", children: _jsx("path", { fill: "#fff", d: "M0 0h159.245v42H0z" }) }) })] }) }));
};
export const _LoadingSpinnerLogo = () => (_jsx("div", { className: "loading p-5 m-5", children: _jsxs("svg", { width: "74", height: "86", viewBox: "0 0 37 43", fill: "none", xmlns: "http://www.w3.org/2000/svg", children: [_jsx("path", { d: "M21.5532 1.77222L18.4825 0L0.0632324 10.6349V31.9031L0.770905 32.3116L3.13387 33.6753L18.4825 42.5379L24.6223 38.9935V35.4475V17.7238L12.3427 10.6349L9.27363 12.4071L21.5532 19.496V37.2197L18.4825 38.9935L3.13387 30.1308V12.4071L18.4825 3.54445L33.8327 12.4071V33.6753L36.9018 31.9031V10.6349L21.5532 1.77222Z", fill: "#511CF7" }), _jsx("path", { d: "M15.4138 8.86255L27.6933 15.9514V37.2196L30.7625 35.4474V14.1792L18.4829 7.08881L15.4138 8.86255Z", fill: "#511CF7" })] }) }));
