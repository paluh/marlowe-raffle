/* jshint -W097 */

'use strict';

import { useWithdrawal as orig } from "../output-es/Component.UseWithdrawal/index.js";
//import React, { useState } from 'react';
//  var [state, setState] = useState({});
//
export const useWithdrawal = function useWithdrawal(props) {
  return orig(props)();
};
