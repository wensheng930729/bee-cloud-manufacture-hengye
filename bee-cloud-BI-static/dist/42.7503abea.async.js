(window["webpackJsonp"]=window["webpackJsonp"]||[]).push([[42],{NS1B:function(e,a,t){"use strict";var r=t("g09b");Object.defineProperty(a,"__esModule",{value:!0}),a["default"]=void 0;var n=r(t("d6i3")),c=r(t("p0pE")),s=t("/TSs"),o={orderInfo:{},tableDatas:{PRODUCT:[],RECEIVE:{},PAY:[]},parentTabs:[],listSearchParams:{currentPage:1,orderStage:"",pageSize:10,searchCount:!0},dataObj:{}},u={namespace:"contractRelated",state:(0,c["default"])({},o),effects:{getListEffect:n["default"].mark(function e(a,t){var r,o,u,i,l,d,p,f,g,h,b,w,m;return n["default"].wrap(function(e){while(1)switch(e.prev=e.next){case 0:return r=a.payload,o=a.callback,u=t.call,i=t.put,e.next=4,u(s.getRelationContract,r);case 4:if(l=e.sent,!l||1!==l.code){e.next=13;break}return d=l.page,p=d.currentPage,f=d.pageSize,g=d.totalRecords,h=l.object,b=h.map(function(e){return(0,c["default"])({key:e.contractBusinessId},e)}),w={list:b,count:h.count,pagination:{total:g,current:p,pageSize:f}},e.next=11,i({type:"getListReduce",payload:{listSearchParams:(0,c["default"])({},r),dataObj:w}});case 11:e.next=14;break;case 13:o&&(m=l&&l.message||"fetchInit-error",o("","error",m));case 14:case"end":return e.stop()}},e)}),machineBindContract:n["default"].mark(function e(a,t){var r,c,o,u;return n["default"].wrap(function(e){while(1)switch(e.prev=e.next){case 0:return r=a.payload,c=a.callback,o=t.call,e.next=4,o(s.machineBindContract,r);case 4:u=e.sent,u&&1===u.code?c("","ok","\u63d0\u4ea4\u6210\u529f"):c("","error",u.message);case 6:case"end":return e.stop()}},e)}),ignoreRecoveryMachine:n["default"].mark(function e(a,t){var r,c,o,u;return n["default"].wrap(function(e){while(1)switch(e.prev=e.next){case 0:return r=a.payload,c=a.callback,o=t.call,e.next=4,o(s.ignoreRecoveryMachine,r);case 4:u=e.sent,u&&1===u.code?c("","ok","\u63d0\u4ea4\u6210\u529f"):c("","error",u.message);case 6:case"end":return e.stop()}},e)})},reducers:{getListReduce:function(e,a){var t=a.payload,r=t.listSearchParams,n=t.dataObj;return(0,c["default"])({},e,{listSearchParams:r,dataObj:n})}}};a["default"]=u}}]);