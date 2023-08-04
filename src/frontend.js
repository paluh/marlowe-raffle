/* jshint -W097 */

'use strict';

import { main } from "../output/Main/index.js";
import domready from 'domready';
import "../public/style.scss";
import 'reactflow/dist/style.css'
import aboutMarkdown from "../public/about.md";
import configBase from "../app-config.json";

const config = {...configBase, aboutMarkdown: aboutMarkdown};

domready(function () {
  main(config)();
});

