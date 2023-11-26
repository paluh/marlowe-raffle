import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
// import { createRoot } from "react-dom/client"
import { ReactFlow, Background, BackgroundVariant, Handle, Position, BaseEdge, getBezierPath, } from "reactflow";
import 'reactflow/dist/style.css';
const X_OFFSET = 200;
const Y_OFFSET = 40;
const NODE_HEIGHT = 30;
const contractNodeStyle = {
    display: "flex",
    flexFlow: "column",
    justifyContent: "center",
    padding: "0px",
    alignItems: "center",
    height: `${NODE_HEIGHT}px`,
    width: "150px",
    background: "white",
    border: "1px solid black",
    borderRadius: "3px",
    fontFamily: "monospace",
};
const nodeTypes = {
    ContractNode({ data: { type, disabled } }) {
        const style_ = { ...contractNodeStyle, opacity: disabled ? "30%" : "100%" };
        if (type === "close")
            return _jsxs("div", { style: style_, children: ["Close", _jsx(Handle, { type: "target", position: Position.Left })] });
        if ("if" in type)
            return _jsxs("div", { style: style_, children: ["If (...)", _jsx(Handle, { type: "target", position: Position.Left, id: "continuation" }), _jsx(Handle, { type: "source", position: Position.Right, style: { top: "8px" }, id: "then" }), _jsx(Handle, { type: "source", position: Position.Right, style: { top: "24px" }, id: "else" })] });
        if ("pay" in type)
            return _jsxs("div", { style: style_, children: ["Pay (...)", _jsx(Handle, { type: "target", position: Position.Left, id: "continuation" }), _jsx(Handle, { type: "source", position: Position.Right, id: "then" })] });
        if ("let" in type)
            return _jsxs("div", { style: style_, children: ["Let (...)", _jsx(Handle, { type: "target", position: Position.Left, id: "continuation" }), _jsx(Handle, { type: "source", position: Position.Right, id: "then" })] });
        if ("assert" in type)
            return _jsxs("div", { style: style_, children: ["Assert (...)", _jsx(Handle, { type: "target", position: Position.Left, id: "continuation" }), _jsx(Handle, { type: "source", position: Position.Right, id: "then" })] });
        if ("when" in type) {
            const height = (type.when.length + 1) * Y_OFFSET - (Y_OFFSET - NODE_HEIGHT);
            const { alignItems: _, ...style } = {
                ...style_,
                height: `${height}px`,
                justifyContent: "space-around",
                paddingLeft: "5px",
            };
            return _jsxs("div", { style: style, children: [_jsx("div", { children: "When" }), type.when.map((x, i) => {
                        if ("merkleized_then" in x) {
                            if ("deposits" in x.case)
                                return _jsx("div", { children: "Deposit (...)" }, i);
                            if ("choose_between" in x.case)
                                return _jsx("div", { children: "Choice (...)" }, i);
                            if ("notify_if" in x.case)
                                return _jsx("div", { children: "Notify (...)" }, i);
                        }
                        else {
                            if ("deposits" in x.case)
                                return _jsxs("div", { children: ["Deposit (...)", _jsx(Handle, { type: "source", position: Position.Right, style: { top: `${i * 32 + 47}px` }, id: `${i}` })] }, i);
                            if ("choose_between" in x.case)
                                return _jsxs("div", { children: ["Choice (...)", _jsx(Handle, { type: "source", position: Position.Right, style: { top: `${i * 32 + 47}px` }, id: `${i}` })] }, i);
                            if ("notify_if" in x.case)
                                return _jsxs("div", { children: ["Notify (...)", _jsx(Handle, { type: "source", position: Position.Right, style: { top: `${i * 32 + 47}px` }, id: `${i}` })] }, i);
                        }
                    }), _jsx("div", { children: "on timeout:" }), _jsx(Handle, { type: "target", position: Position.Left, style: { top: `${NODE_HEIGHT / 2}px` }, id: "continuation" }), _jsx(Handle, { type: "source", position: Position.Right, style: { top: `${height - NODE_HEIGHT / 2}px` }, id: "timeout_continuation" })] });
        }
        return _jsx("div", { style: style_, children: "Not implemented!" });
    }
};
const edgeTypes = {
    ContractEdge({ data, sourceX, sourceY, sourcePosition, targetX, targetY, targetPosition, markerEnd, style = {} }) {
        const [edgePath] = getBezierPath({ sourceX, sourceY, sourcePosition, targetX, targetY, targetPosition });
        const style_ = data && data.disabled
            ? { ...style, strokeOpacity: "30%" }
            : {
                ...style, strokeOpacity: "100%",
                strokeWidth: 3,
                // strokeDasharray: "5",
                // strokeDashoffset: "0",
                // animation: "edgeflow 1000ms linear infinite",
            };
        return _jsx(BaseEdge, { path: edgePath, markerEnd: markerEnd, style: style_ });
    }
};
const contract2NodesAndEdges = (contract, id, x, y, path) => {
    if (contract === "close")
        return {
            nodes: [{ id, position: { x, y }, data: { type: "close", disabled: false }, type: "ContractNode" }],
            edges: [],
            max_y: y,
        };
    if ("if" in contract) {
        const [index, ...indices] = path;
        const { else: else_, then, ...type } = contract;
        const then_id = `1-${id}`;
        const else_id = `2-${id}`;
        const { max_y: then_max_y, nodes: then_nodes, edges: then_edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y, indices);
        const { max_y, nodes: else_nodes, edges: else_edges } = contract2NodesAndEdges(else_, else_id, x + X_OFFSET, then_max_y + Y_OFFSET, indices);
        return {
            nodes: [
                ...then_nodes.map(node => index === 0 || index === undefined ? node : { ...node, data: { ...node.data, disabled: true } }),
                ...else_nodes.map(node => index === 1 || index === undefined ? node : { ...node, data: { ...node.data, disabled: true } }),
                { id, position: { x, y }, data: { type, disabled: false }, type: "ContractNode" },
            ],
            edges: [
                ...then_edges.map(edge => index === 0 || index === undefined ? edge : { ...edge, data: edge.data && { ...edge.data, disabled: true } }),
                ...else_edges.map(edge => index === 1 || index === undefined ? edge : { ...edge, data: edge.data && { ...edge.data, disabled: true } }),
                { id: `then-${id}`, source: id, target: then_id, sourceHandle: "then", type: "ContractEdge", data: { disabled: !(index === 0 || index === undefined) } },
                { id: `else-${id}`, source: id, target: else_id, sourceHandle: "else", type: "ContractEdge", data: { disabled: !(index === 1 || index === undefined) } },
            ],
            max_y,
        };
    }
    if ("pay" in contract) {
        const { then, ...type } = contract;
        const then_id = `1-${id}`;
        const { max_y, nodes, edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y, path);
        return {
            nodes: [
                ...nodes,
                { id, position: { x, y }, data: { type, disabled: false }, type: "ContractNode" },
            ],
            edges: [
                ...edges,
                { id: `then-${id}`, source: id, target: then_id, type: "ContractEdge", data: { disabled: false } },
            ],
            max_y,
        };
    }
    if ("let" in contract) {
        const { then, ...type } = contract;
        const then_id = `1-${id}`;
        const { max_y, nodes, edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y, path);
        return {
            nodes: [
                ...nodes,
                { id, position: { x, y }, data: { type, disabled: false }, type: "ContractNode" },
            ],
            edges: [
                ...edges,
                { id: `then-${id}`, source: id, target: then_id, type: "ContractEdge", data: { disabled: false } },
            ],
            max_y,
        };
    }
    if ("assert" in contract) {
        const { then, ...type } = contract;
        const then_id = `1-${id}`;
        const { max_y, nodes, edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y, path);
        return {
            nodes: [
                ...nodes,
                { id, position: { x, y }, data: { type, disabled: false }, type: "ContractNode" },
            ],
            edges: [
                ...edges,
                { id: `then-${id}`, source: id, target: then_id, type: "ContractEdge", data: { disabled: false } },
            ],
            max_y,
        };
    }
    if ("when" in contract) {
        const [index, ...indices] = path;
        const { timeout_continuation, ...type } = contract;
        const { nodes, edges, max_y } = type.when.reduce((acc, on, i) => {
            if ("then" in on) {
                const then_id = `${i}-${id}`;
                const { nodes, edges, max_y } = contract2NodesAndEdges(on.then, then_id, x + X_OFFSET, acc.max_y + Y_OFFSET, indices);
                return {
                    nodes: [
                        ...nodes.map(node => index === i || index === undefined ? node : { ...node, data: { ...node.data, disabled: true } }),
                        ...acc.nodes,
                    ],
                    edges: [
                        ...edges.map(edge => index === i || index === undefined ? edge : { ...edge, data: edge.data && { ...edge.data, disabled: true } }),
                        ...acc.edges,
                        { id: `then-${then_id}-${id}`, source: id, target: then_id, sourceHandle: `${i}`, type: "ContractEdge", data: { disabled: !(index === i || index === undefined) } },
                    ],
                    max_y
                };
            }
            return acc;
        }, {
            nodes: [
                { id, position: { x, y }, data: { type, disabled: false }, type: "ContractNode" },
            ],
            edges: [],
            max_y: y - Y_OFFSET
        });
        const timeout_then_id = `${type.when.length}-${id}`;
        const timeout_graph = contract2NodesAndEdges(timeout_continuation, timeout_then_id, x + X_OFFSET, max_y + Y_OFFSET, indices);
        return {
            nodes: [
                ...timeout_graph.nodes.map(node => index === null || index === undefined ? node : { ...node, data: { ...node.data, disabled: true } }),
                ...nodes,
            ],
            edges: [
                ...timeout_graph.edges.map(edge => index === null || index === undefined ? edge : { ...edge, data: edge.data && { ...edge.data, disabled: true } }),
                ...edges,
                { id: `timeout_continuation-${id}`, source: id, target: timeout_then_id, sourceHandle: "timeout_continuation", type: "ContractEdge", data: { disabled: !(index === null || index === undefined) } },
            ],
            max_y: timeout_graph.max_y
        };
    }
    return {
        nodes: [],
        edges: [],
        max_y: y,
    };
};
export const _MarloweGraph = ({ contract, path }) => {
    const { nodes, edges } = contract2NodesAndEdges(contract, "1", 0, 0, path || []);
    return _jsx(ReactFlow, { nodes: nodes, edges: edges, nodeTypes: nodeTypes, edgeTypes: edgeTypes, children: _jsx(Background, { variant: BackgroundVariant.Dots, gap: 12, size: 1 }) });
};
