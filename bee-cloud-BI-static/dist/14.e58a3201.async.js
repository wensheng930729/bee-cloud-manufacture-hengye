(window["webpackJsonp"]=window["webpackJsonp"]||[]).push([[14],{Y2Zh:function(e,t,r){"use strict";var a=r("g09b");Object.defineProperty(t,"__esModule",{value:!0}),t["default"]=void 0;var n=a(r("Y/ft")),c=a(r("d6i3")),i=a(r("p0pE")),o=a(r("qIgq")),l=a(r("wd/R")),s=r("LvDl"),u=r("x5D9"),p={namespace:"TransportSectionDetailModel",state:{carrayNameOptions:[],requestType:"buyTransportSection",transportSectionDetail:{transportModeName:"",toFactory:0,startingPlace:"",arrivalPlace:""},carrierTransportDTOS:[],collapseActivityKey:"",carrierTransportDetailOBJ:{},carrierTransportDetailOBJOrigin:{}},effects:{getTransportSectionAllDetailEffect:c["default"].mark(function e(t,r){var a,n,l,s,p,d,f,y,T,D;return c["default"].wrap(function(e){while(1)switch(e.prev=e.next){case 0:return a=t.payload,n=t.callback,l=r.call,s=r.all,p=r.put,e.next=4,s([l(u.getTransportSectionAllDetail,a),l(u.getCarrierInfoList,{})]);case 4:if(d=e.sent,f=(0,o["default"])(d,2),y=f[0],T=f[1],!y||!T){e.next=18;break}if(1!==y.code||1!==T.code){e.next=14;break}return e.next=12,p({type:"getTransportSectionAllDetailReduce",payload:(0,i["default"])({},y.object,{requestType:a.requestType,carrayNameList:T.object})});case 12:e.next=16;break;case 14:D=y.message||T.message||"getTransportSectionAllDetail-error",n("","error",D);case 16:e.next=19;break;case 18:n&&n("","error","getTransportSectionAllDetail-error");case 19:case"end":return e.stop()}},e)}),addTransportDetailListEffect:c["default"].mark(function e(t,r){var a,n,i,o,l,u,p,d,f;return c["default"].wrap(function(e){while(1)switch(e.prev=e.next){case 0:return a=t.payload,n=r.put,i=r.select,o=a.id,e.next=5,i(function(e){return e.TransportSectionDetailModel});case 5:return l=e.sent,u=l.carrierTransportDetailOBJ,p=(0,s.cloneDeep)(u),d=p[o]||[],f="".concat((0,s.random)(0,9999),"_id"),d.push({key:f,id:f,trainNumber:"",driver:"",contact:"",cargoWeight:"",editable:!0}),p[o]=d,e.next=14,n({type:"cacheTransportDetailListEffect",payload:p});case 14:case"end":return e.stop()}},e)}),deleteTransportDetailListEffect:c["default"].mark(function e(t,r){var a,n,i,o,l,u,p,d,f,y;return c["default"].wrap(function(e){while(1)switch(e.prev=e.next){case 0:return a=t.payload,n=r.put,i=r.select,o=a.collapseActivityKey,l=a.id,e.next=5,i(function(e){return e.TransportSectionDetailModel});case 5:return u=e.sent,p=u.carrierTransportDetailOBJ,d=(0,s.cloneDeep)(p),f=d[o],y=f.filter(function(e){return e.id!==l}),d[o]=y,e.next=13,n({type:"cacheTransportDetailListEffect",payload:d});case 13:case"end":return e.stop()}},e)}),cacheTransportDetailListEffect:c["default"].mark(function e(t,r){var a,n,i,o,l,u;return c["default"].wrap(function(e){while(1)switch(e.prev=e.next){case 0:if(a=t.payload,n=r.put,i=r.select,o=a,o){e.next=9;break}return e.next=6,i(function(e){return e.TransportSectionDetailModel});case 6:l=e.sent,u=l.carrierTransportDetailOBJOrigin,o=(0,s.cloneDeep)(u);case 9:return e.next=11,n({type:"cacheTransportDetailListReduce",payload:o});case 11:case"end":return e.stop()}},e)}),cacheCollapseActivityKeyEffect:c["default"].mark(function e(t,r){var a,n;return c["default"].wrap(function(e){while(1)switch(e.prev=e.next){case 0:return a=t.payload,n=r.put,e.next=4,n({type:"cacheCollapseActivityKeyReduce",payload:a});case 4:case"end":return e.stop()}},e)}),addCarrayListEffect:c["default"].mark(function e(t,r){var a,n,i,o,u,p;return c["default"].wrap(function(e){while(1)switch(e.prev=e.next){case 0:return t.payload,a=r.put,n=r.select,e.next=4,n(function(e){return e.TransportSectionDetailModel});case 4:return i=e.sent,o=i.carrierTransportDTOS,u=i.carrayNameOptions,p=(0,s.cloneDeep)(o),p.push({id:"".concat((0,s.random)(999,9999),"_id"),carrierId:u[0].value,unitPrice:"",carriage:"",departureTime:(0,l["default"])().format("YYYY-MM-DD HH:mm:ss"),estimateArrivalTime:(0,l["default"])().format("YYYY-MM-DD HH:mm:ss")}),e.next=11,a({type:"cacheCarrayListReduce",payload:p});case 11:case"end":return e.stop()}},e)}),saveCarrayListEffect:c["default"].mark(function e(t,r){var a,o,l,s,p,d,f,y,T,D,v,h,m,x,S,b,w;return c["default"].wrap(function(e){while(1)switch(e.prev=e.next){case 0:return t.payload,a=t.callback,o=r.select,r.put,l=r.call,e.next=4,o(function(e){return e.TransportSectionDetailModel});case 4:if(s=e.sent,p=s.transportSectionDetail,d=s.carrierTransportDTOS,f=s.carrierTransportDetailOBJ,y=s.collapseActivityKey,T=s.requestType,D=f[y],D&&0!==D.length){e.next=15;break}a("","error","\u6682\u65e0\u6570\u636e\u53ef\u4fdd\u5b58"),e.next=31;break;case 15:if(v=D.filter(function(e){return!e.disabled}),0!==v.length){e.next=19;break}return a("","error","\u6682\u65e0\u6570\u636e\u53ef\u4fdd\u5b58"),e.abrupt("return");case 19:if(h=v.map(function(e,t){e.editable,e.key,e.id;var r=(0,n["default"])(e,["editable","key","id"]);return(0,i["default"])({},r)}),m=d.filter(function(e){return"".concat(e.id)==="".concat(y)}),x=h.filter(function(e){return!e.trainNumber||!e.cargoWeight}),!(x.length>0)){e.next=26;break}a("","error","\u8fd0\u8f7d\u5355\u4f4d\u7f16\u53f7\u4e0e\u5428\u4f4d\u5fc5\u586b"),e.next=31;break;case 26:return S=(0,i["default"])({},p,{carrierTransportDTOS:[(0,i["default"])({},m[0],{id:"".concat(m[0].id).includes("id")?"":m[0].id,detailDTOS:h})],requestType:T}),e.next=29,l(u.saveTransportSection,S);case 29:b=e.sent,b&&1===b.code?a("","ok","\u4fdd\u5b58\u627f\u8fd0\u5546\u53ca\u8fd0\u8f7d\u5355\u4f4d\u6210\u529f"):a&&(w=b&&b.message||"saveTransportSection-error",a("","error",w));case 31:case"end":return e.stop()}},e)})},reducers:{getTransportSectionAllDetailReduce:function(e,t){var r=t.payload,a=r.carrierTransportDTOS,c=r.requestType,o=r.carrayNameList,l=(0,n["default"])(r,["carrierTransportDTOS","requestType","carrayNameList"]),s=o.map(function(e){return{value:"".concat(e.carrierId),label:e.carrierName}}),u=(0,i["default"])({},l),p=[],d="",f={};return a.forEach(function(e){e.detailDTOS;var t=(0,n["default"])(e,["detailDTOS"]);p.push(t),f[e.id]=e.detailDTOS.map(function(e){return(0,i["default"])({},e,{key:e.id,editable:!0,disabled:!0})})}),p.length>0&&(d="".concat(p[0].id)),(0,i["default"])({},e,{carrayNameOptions:s,requestType:c,transportSectionDetail:u,carrierTransportDTOS:p,collapseActivityKey:d,carrierTransportDetailOBJ:f,carrierTransportDetailOBJOrigin:f})},cacheTransportDetailListReduce:function(e,t){var r=t.payload;return(0,i["default"])({},e,{carrierTransportDetailOBJ:r})},cacheCollapseActivityKeyReduce:function(e,t){var r=t.payload;return(0,i["default"])({},e,{collapseActivityKey:r})},cacheCarrayListReduce:function(e,t){var r=t.payload;return(0,i["default"])({},e,{carrierTransportDTOS:r})}}};t["default"]=p}}]);