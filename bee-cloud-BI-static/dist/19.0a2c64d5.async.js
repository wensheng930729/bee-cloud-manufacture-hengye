(window["webpackJsonp"]=window["webpackJsonp"]||[]).push([[19],{"5Qr/":function(e,t,a){"use strict";var l=a("g09b");Object.defineProperty(t,"__esModule",{value:!0}),t["default"]=void 0;var n=l(a("Up7M")),u=l(a("IH53")),r=l(a("WrNr")),c={Detail:n["default"],Receive:u["default"],Files:r["default"]};t["default"]=c},"84xA":function(e,t,a){e.exports={container:"container___1Bzwt"}},CCpI:function(e,t,a){"use strict";var l=a("g09b"),n=a("tAuX");Object.defineProperty(t,"__esModule",{value:!0}),t["default"]=void 0,a("IzEo");var u=l(a("bx4M"));a("P2fV");var r=l(a("NJEC"));a("+L6B");var c=l(a("2/Rp"));a("14J3");var d=l(a("BMrR"));a("jCWc");var s=l(a("kPKH"));a("miYZ");var o=l(a("tsqr"));a("iQDF");var i=l(a("+eQT"));a("y8nQ");var f=l(a("Vl3Y"));a("giR+");var m,p,h,v=l(a("fyUT")),E=l(a("2Taf")),y=l(a("vZ4D")),b=l(a("l4Ni")),g=l(a("ujKo")),C=l(a("MhPg")),k=n(a("q1tI")),I=a("MuoO"),_=l(a("kePK")),D=n(a("f97x")),P=a("4rMY"),T=a("Kvkj"),w=l(a("wd/R")),M=l(a("bKel")),N=(m=(0,I.connect)(function(e){var t=e.purchase.orderInfo;return{orderInfo:t}}),(0,M["default"])(p=m((h=function(e){function t(){var e,a;(0,E["default"])(this,t);for(var l=arguments.length,n=new Array(l),u=0;u<l;u++)n[u]=arguments[u];return a=(0,b["default"])(this,(e=(0,g["default"])(t)).call.apply(e,[this].concat(n))),a.state={visible:!1},a.handleOk=function(e){var t=e.payAmount,l=e.payTime,n={payAmount:t,payTime:(0,w["default"])(l).format("YYYY-MM-DD"),contractBusinessId:a.contractBusinessId};(0,P.payForBuyContractBasic)(n).then(function(e){1===e.code&&(a.setState({visible:!1}),a.props.dispatch({type:"purchase/getBuyContractDetail",payload:a.contractBusinessId})),o["default"].info(e.message)})},a}return(0,C["default"])(t,e),(0,y["default"])(t,[{key:"componentDidMount",value:function(){this.contractBusinessId=this.props.location.query.contractBusinessId}},{key:"completeBuyContract",value:function(e){var t=this;(0,P.completeBuyContract)({businessId:e}).then(function(e){1===e.code&&t.props.dispatch({type:"purchase/getBuyContractDetail",payload:t.contractBusinessId}),o["default"].info(e.message)})}},{key:"showPay",value:function(e){this.setState({visible:e})}},{key:"render",value:function(){var e=this.state.visible,t=this.props,a=t.tabList,l=t.children,n=t.orderInfo.contract,o=void 0===n?{}:n,i=t.activeKey,f=t.onTabChange,m={visible:e,title:"\u4ed8\u6b3e",onOk:this.handleOk,onCancel:this.showPay.bind(this,!1),width:700,okText:"\u786e\u5b9a\u4ed8\u6b3e",cancelText:"\u53d6\u6d88"};return k["default"].createElement("div",{className:_["default"].container},k["default"].createElement(d["default"],null,k["default"].createElement(s["default"],{span:16,className:_["default"].left},k["default"].createElement("div",{className:_["default"].title}," \u5408\u540c\u7f16\u53f7\uff1a",o.contractNum),k["default"].createElement(d["default"],{className:_["default"].info},k["default"].createElement(s["default"],{span:10},k["default"].createElement("span",null,"\u3000\u521b\u5efa\u4eba\uff1a"),k["default"].createElement("span",null,o.creator)),k["default"].createElement(s["default"],{span:10},k["default"].createElement("span",null,"\u3000\u4f9b\u5e94\u5546\uff1a"),k["default"].createElement("span",null,o.supplierName)),k["default"].createElement(s["default"],{span:10},k["default"].createElement("span",null,"\u7b7e\u8ba2\u65e5\u671f\uff1a"),k["default"].createElement("span",null,o.signDate)),k["default"].createElement(s["default"],{span:10},k["default"].createElement("span",null,"\u3000\u8d77\u59cb\u5730\uff1a"),k["default"].createElement("span",null,o.originAddress)),k["default"].createElement(s["default"],{span:10},k["default"].createElement("span",null,"\u3000\u8054\u7cfb\u4eba\uff1a"),k["default"].createElement("span",null,o.linkMan)),k["default"].createElement(s["default"],{span:10},k["default"].createElement("span",null,"\u91c7\u8d2d\u65b9\u5f0f\uff1a"),k["default"].createElement("span",null,D.purchaserMode[o.purchaserMode||"0"])))),k["default"].createElement("div",{className:_["default"].right},0===o.completed&&"Detail"===i?k["default"].createElement("div",{className:_["default"].options},k["default"].createElement(r["default"],{title:"\u786e\u5b9a\u5b8c\u6210\u8be5\u5408\u540c\u5417?",onConfirm:this.completeBuyContract.bind(this,o.contractBusinessId),okText:"\u786e\u5b9a",cancelText:"\u53d6\u6d88"},k["default"].createElement(c["default"],null,"\u5b8c\u6210\u5408\u540c")),k["default"].createElement(c["default"],{type:"primary",onClick:this.showPay.bind(this,!0)},"\u4ed8\u6b3e")):null,k["default"].createElement("div",{className:_["default"].statistics},k["default"].createElement("div",null,k["default"].createElement("span",null,"\u72b6\u6001"),k["default"].createElement("span",null,D.completed[o.completed])),k["default"].createElement("div",null,k["default"].createElement("span",null,"\u5408\u540c\u91d1\u989d"),k["default"].createElement("span",null,"\xa5 ",o.amount||0))))),k["default"].createElement(u["default"],{className:_["default"].tabCard,tabList:a,activeTabKey:i,onTabChange:function(e){f(e)}},l),k["default"].createElement(O,m))}}]),t}(k.PureComponent),p=h))||p)||p);t["default"]=N;var x=function(e){function t(){return(0,E["default"])(this,t),(0,b["default"])(this,(0,g["default"])(t).apply(this,arguments))}return(0,C["default"])(t,e),(0,y["default"])(t,[{key:"render",value:function(){var e=this.props.form.getFieldDecorator,t={labelCol:{xs:{span:24},sm:{span:8}},wrapperCol:{xs:{span:24},sm:{span:16}}},a={rules:[{required:!0}]};return k["default"].createElement(f["default"],t,k["default"].createElement(f["default"].Item,{label:"\u4ed8\u6b3e\u91d1\u989d"},e("payAmount",a)(k["default"].createElement(v["default"],null))),k["default"].createElement(f["default"].Item,{label:"\u4ed8\u6b3e\u65f6\u95f4"},e("payTime",a)(k["default"].createElement(i["default"],null))))}}]),t}(k.PureComponent),O=(0,T.MadalHOC)(f["default"].create()(x))},DoeO:function(e,t,a){"use strict";var l=a("g09b"),n=a("tAuX");Object.defineProperty(t,"__esModule",{value:!0}),t["default"]=void 0;var u,r,c,d=l(a("2Taf")),s=l(a("vZ4D")),o=l(a("l4Ni")),i=l(a("ujKo")),f=l(a("MhPg")),m=l(a("jHYr")),p=n(a("q1tI")),h=l(a("CCpI")),v=l(a("84xA")),E=l(a("bKel")),y=l(a("5Qr/")),b=a("MuoO"),g=(u=(0,b.connect)(function(e){return(0,m["default"])(e),{}}),(0,E["default"])(r=u((c=function(e){function t(){var e,a;(0,d["default"])(this,t);for(var l=arguments.length,n=new Array(l),u=0;u<l;u++)n[u]=arguments[u];return a=(0,o["default"])(this,(e=(0,i["default"])(t)).call.apply(e,[this].concat(n))),a.state={tabList:[{tab:"\u8be6\u60c5",key:"Detail"},{tab:"\u6536\u8d27\u60c5\u51b5",key:"Receive"},{tab:"\u5408\u540c\u9644\u4ef6",key:"Files"}],activeKey:"Detail"},a.onTabChange=function(e){a.setState({activeKey:e})},a}return(0,f["default"])(t,e),(0,s["default"])(t,[{key:"componentDidMount",value:function(){this.contractBusinessId=this.props.location.query.contractBusinessId;var e=this.props.dispatch;e({type:"purchase/getBuyContractDetail",payload:this.contractBusinessId})}},{key:"render",value:function(){var e=this.state,t=e.tabList,a=e.activeKey,l=y["default"][a];return p["default"].createElement("div",{className:v["default"].container},p["default"].createElement(h["default"],{tabList:t,activeKey:a,onTabChange:this.onTabChange},l?p["default"].createElement(l,null):null))}}]),t}(p.Component),r=c))||r)||r);t["default"]=g},IH53:function(e,t,a){"use strict";var l=a("g09b"),n=a("tAuX");Object.defineProperty(t,"__esModule",{value:!0}),t["default"]=void 0,a("lUTK");var u=l(a("BvKs"));a("qVdP");var r=l(a("jsC+"));a("Pwec");var c=l(a("CtXQ"));a("P2fV");var d=l(a("NJEC")),s=l(a("gWZ8")),o=l(a("p0pE"));a("miYZ");var i=l(a("tsqr"));a("giR+");var f=l(a("fyUT"));a("y8nQ");var m=l(a("Vl3Y")),p=l(a("2Taf")),h=l(a("vZ4D")),v=l(a("l4Ni")),E=l(a("ujKo")),y=l(a("MhPg"));a("IzEo");var b=l(a("bx4M"));a("g9YV");var g=l(a("wCAj"));a("2qtc");var C,k,I,_=l(a("kLXV")),D=n(a("q1tI")),P=a("MuoO"),T=l(a("bKel")),w=l(a("J7Xc")),M=l(a("AwN5")),N=a("Kvkj"),x=a("4rMY"),O=_["default"].confirm,R=(C=(0,P.connect)(function(e){var t=e.purchase,a=t.tableDatas,l=t.orderInfo;return{tableDatas:a,orderInfo:l}}),(0,T["default"])(k=C((I=function(e){function t(){var e,a;(0,p["default"])(this,t);for(var l=arguments.length,n=new Array(l),u=0;u<l;u++)n[u]=arguments[u];return a=(0,v["default"])(this,(e=(0,E["default"])(t)).call.apply(e,[this].concat(n))),a.state={sampleInfo:{},showReslut:!1,discountInfo:{},showDiscount:!1},a.bulkConfirmProduct=function(e){(0,x.bulkConfirmProduct)([{machineId:e}]).then(function(e){1===e.code?i["default"].success(e.message,1,function(){return a.getData({})}):i["default"].error(e.message)})},a.bulkDiscountedProduct=function(e,t){var l=(0,o["default"])({},t,{machineIds:[e.machineId]});(0,x.bulkDiscountedProduct)(l).then(function(e){1===e.code&&(a.getData({}),a.setState({showDiscount:!1})),i["default"].info(e.message)})},a}return(0,y["default"])(t,e),(0,h["default"])(t,[{key:"componentDidMount",value:function(){this.contractBusinessId=this.props.location.query.contractBusinessId,this.getData({})}},{key:"getData",value:function(e){var t=e.currentPage,a=void 0===t?1:t,l=this.props.dispatch;l({type:"purchase/getCarList",payload:{contractBusinessId:this.contractBusinessId,currentPage:a,pageSize:5}})}},{key:"onChange",value:function(e){var t=e.current;this.getData({currentPage:t})}},{key:"showResult",value:function(e){var t=this;(0,x.getCarSampleInfo)({machineId:e}).then(function(e){1===e.code&&t.setState({sampleInfo:e.object,showReslut:!0})})}},{key:"purchaserReAssay",value:function(e){var t=this;(0,x.purchaserReAssay)({machineId:e}).then(function(e){1===e.code?(t.getData({}),i["default"].success(e.message)):i["default"].error(e.message)})}},{key:"moreOptionClick",value:function(e,t){var a=t.key,l=this;switch(a){case"1":O({title:"\u8be5\u8f66\u6b21\u5165\u5e93\u540e\u65e0\u6cd5\u4fee\u6539",content:"\u662f\u5426\u7ee7\u7eed\u5165\u5e93\uff1f",okText:"\u786e\u5b9a",cancelText:"\u53d6\u6d88",onOk:function(){l.bulkConfirmProduct(e.machineId)},onCancel:function(){}});break;case"2":this.setState({discountInfo:e,showDiscount:!0});break;default:break}}},{key:"render",value:function(){var e=this,t=this.state,a=t.showReslut,l=t.sampleInfo,n=t.showDiscount,i=t.discountInfo,f=this.props,m=f.tableDatas,p=f.orderInfo.contract,h=void 0===p?{}:p,v=m[M["default"].RECEIVE.key],E=v.data,y=void 0===E?[]:E,b=v.page,g=void 0===b?{}:b,C={current:g.currentPage||1,total:g.totalRecords||0,pageSize:g.pageSize||6},k={visible:a,title:"\u67e5\u770b\u5316\u9a8c\u7ed3\u679c",onOk:function(){return e.setState({showReslut:!1})},onCancel:function(){return e.setState({showReslut:!1})},width:700,okText:"\u786e\u5b9a",cancelText:"\u53d6\u6d88",ComProps:{data:l}},I={visible:n,title:"\u6298\u4ef7\u5165\u5e93",onOk:this.bulkDiscountedProduct.bind(this,i),onCancel:function(){return e.setState({showDiscount:!1})},width:700,okText:"\u786e\u5b9a",cancelText:"\u53d6\u6d88",ComProps:{data:(0,o["default"])({},i,h)}},_=(0,s["default"])(M["default"].RECEIVE.columns);_.push({title:"\u64cd\u4f5c",align:"center",dataIndex:"_option",key:"_option",render:function(t,a){return D["default"].createElement(D["default"].Fragment,null,D["default"].createElement("a",{onClick:e.showResult.bind(e,a.machineId)},"\u67e5\u770b\u5316\u9a8c\u7ed3\u679c"),"\u3000\xa0",D["default"].createElement(d["default"],{title:"\u786e\u5b9a\u518d\u6b21\u5316\u9a8c\u8be5\u6837\u54c1\u5417?",onConfirm:e.purchaserReAssay.bind(e,a.machineId),okText:"\u786e\u5b9a",cancelText:"\u53d6\u6d88"},D["default"].createElement("a",null,"\u518d\u6b21\u5316\u9a8c")),"\u3000\xa0",void 0===a.handleType?D["default"].createElement(r["default"],{overlay:P(a)},D["default"].createElement("a",{className:"ant-dropdown-link"},"\u66f4\u591a\u64cd\u4f5c ",D["default"].createElement(c["default"],{type:"down"}))):null," ")}});var P=function(t){return D["default"].createElement(u["default"],{onClick:e.moreOptionClick.bind(e,t)},D["default"].createElement(u["default"].Item,{key:1},D["default"].createElement("a",{target:"_blank"},"\u786e\u8ba4\u5165\u5e93")),D["default"].createElement(u["default"].Item,{key:2},D["default"].createElement("a",{target:"_blank"},"\u6298\u4ef7\u5165\u5e93")))};return D["default"].createElement(D["default"].Fragment,null,D["default"].createElement(B,{columns:_,data:y,pagination:C,name:M["default"].RECEIVE.name,onChange:this.onChange.bind(this)}),D["default"].createElement(K,k),D["default"].createElement(q,I))}}]),t}(D.PureComponent),k=I))||k)||k);t["default"]=R;var B=function(e){var t=e.name,a=e.columns,l=e.data,n=e.pagination,u=e.onChange;return D["default"].createElement(b["default"],{title:t,className:w["default"].cardTable},D["default"].createElement(g["default"],{columns:a,dataSource:l,pagination:n,onChange:u}))},A=function(e){var t=e.data,a=void 0===t?{}:t;return D["default"].createElement("div",{className:w["default"].result},D["default"].createElement("section",null,D["default"].createElement("span",null,"\u5408\u540c\u7f16\u53f7\uff1a",a.contractNum),D["default"].createElement("span",null,"\u4ea7\u54c1\u540d\u79f0\uff1a",a.productName),D["default"].createElement("span",null,"\u8d28\u91cf\u8981\u6c42\uff1a",a.qualityRequirement)),D["default"].createElement(D["default"].Fragment,null,a.sampleResultDTOs&&a.sampleResultDTOs.length?a.sampleResultDTOs.map(function(e){return D["default"].createElement("section",null,D["default"].createElement("span",null,"\u6837\u54c1\u7f16\u53f7\uff1a",e.sampleCode),D["default"].createElement("span",null,"\u3000\u3000\u89c4\u683c\uff1a",e.productSpecName),D["default"].createElement("span",null,"\u53d6\u6837\u65f6\u95f4\uff1a",e.createTime),D["default"].createElement("span",null,"\u5316\u9a8c\u65f6\u95f4\uff1a",e.assayTime),D["default"].createElement("span",null,"\u5316\u9a8c\u7ed3\u679c\uff1a",D["default"].createElement("ul",null,e.assays&&e.assays.length?e.assays.map(function(e){return D["default"].createElement(D["default"].Fragment,null,D["default"].createElement("li",null,D["default"].createElement("div",null,e.assayItem),D["default"].createElement("div",null,e.assayValue+""+e.unitString)))}):null," ")," "))}):null))},K=(0,N.MadalHOC)(A),j=function(e){function t(){return(0,p["default"])(this,t),(0,v["default"])(this,(0,E["default"])(t).apply(this,arguments))}return(0,y["default"])(t,e),(0,h["default"])(t,[{key:"render",value:function(){var e=this.props,t=e.data,a=e.form.getFieldDecorator,l={labelCol:{xs:{span:24},sm:{span:8}},wrapperCol:{xs:{span:24},sm:{span:16}}},n={rules:[{required:!0}]};return D["default"].createElement(m["default"],l,D["default"].createElement(m["default"].Item,{label:"\u4f9b\u5e94\u5546"},t.supplierName),D["default"].createElement(m["default"].Item,{label:"\u4ea7\u54c1\u540d\u79f0"},t.productName),D["default"].createElement(m["default"].Item,{label:"\u51c0\u91cd"},t.cargoWeight||0,"\u5428"),D["default"].createElement(m["default"].Item,{label:"\u6298\u540e\u5355\u4ef7"},a("discountedPrice",n)(D["default"].createElement(f["default"],null))))}}]),t}(D.PureComponent),q=(0,N.MadalHOC)(m["default"].create()(j))},J7Xc:function(e,t,a){e.exports={flex:"flex___3i-Y_",cardTable:"cardTable___1iiKH",result:"result___q1Ftl"}},Up7M:function(e,t,a){"use strict";var l=a("g09b"),n=a("tAuX");Object.defineProperty(t,"__esModule",{value:!0}),t["default"]=void 0;var u=l(a("2Taf")),r=l(a("vZ4D")),c=l(a("l4Ni")),d=l(a("ujKo")),s=l(a("MhPg"));a("IzEo");var o=l(a("bx4M"));a("g9YV");var i,f,m=l(a("wCAj")),p=n(a("q1tI")),h=a("MuoO"),v=l(a("e6V8")),E=l(a("AwN5")),y=(i=(0,h.connect)(function(e){var t=e.purchase,a=t.orderInfo,l=t.tableDatas;return{orderInfo:a,tableDatas:l}}),i(f=function(e){function t(){return(0,u["default"])(this,t),(0,c["default"])(this,(0,d["default"])(t).apply(this,arguments))}return(0,s["default"])(t,e),(0,r["default"])(t,[{key:"render",value:function(){var e=this.props,t=e.orderInfo.contract,a=void 0===t?{}:t,l=e.tableDatas;return p["default"].createElement(p["default"].Fragment,null,p["default"].createElement(o["default"],{title:"\u7269\u6d41\u4fe1\u606f"},p["default"].createElement("div",{className:v["default"].flex},p["default"].createElement("span",null,"\u5408\u540c\u6570\u91cf\uff1a",a.quantity||"0","\u5428")," ",p["default"].createElement("span",null,"\u5df2\u53d1\u91cf\uff1a",a.issuedVolume||"0","\u5428"),p["default"].createElement("span",null,"\u5728\u9014\u91cf\uff1a",a.trafficVolume||"0","\u5428")," ",p["default"].createElement("span",null,"\u5df2\u5230\u91cf\uff1a",a.arrivalVolume||"0","\u5428"),p["default"].createElement("span",null,"\u672a\u53d1\u91cf\uff1a",a.undeliveredVolume||"0","\u5428"))),p["default"].createElement(b,{columns:E["default"].PRODUCT.columns,data:l[E["default"].PRODUCT.key],name:E["default"].PRODUCT.name}),p["default"].createElement(b,{columns:E["default"].PAY.columns,data:l[E["default"].PAY.key],name:E["default"].PAY.name}))}}]),t}(p.Component))||f);t["default"]=y,y.defaultProps={activeKey:"",tabList:[],onTabChange:function(){return null}};var b=function(e){var t=e.name,a=e.columns,l=e.data;return p["default"].createElement(o["default"],{title:t,className:v["default"].cardTable},p["default"].createElement(m["default"],{pagination:!1,columns:a,dataSource:l}))}},WrNr:function(e,t,a){"use strict";var l=a("g09b"),n=a("tAuX");Object.defineProperty(t,"__esModule",{value:!0}),t["default"]=void 0,a("IzEo");var u=l(a("bx4M"));a("R9oj");var r=l(a("ECub"));a("Pwec");var c,d,s,o=l(a("CtXQ")),i=l(a("2Taf")),f=l(a("vZ4D")),m=l(a("l4Ni")),p=l(a("ujKo")),h=l(a("MhPg")),v=n(a("q1tI")),E=a("MuoO"),y=l(a("ulGV")),b=(c=(0,E.connect)(function(e){var t=e.purchase.orderInfo;return{orderInfo:t}}),c((s=function(e){function t(){var e,a;(0,i["default"])(this,t);for(var l=arguments.length,n=new Array(l),u=0;u<l;u++)n[u]=arguments[u];return a=(0,m["default"])(this,(e=(0,p["default"])(t)).call.apply(e,[this].concat(n))),a.handleSkip=function(e){e&&window.open(e)},a}return(0,h["default"])(t,e),(0,f["default"])(t,[{key:"render",value:function(){var e=this,t=this.props.orderInfo;return v["default"].createElement("div",{className:y["default"].container},v["default"].createElement(u["default"],{title:"\u5408\u540c\u9644\u4ef6"},t&&t.contract&&t.contract.files&&t.contract.files.length?t.contract.files.map(function(t,a){return v["default"].createElement("div",{className:y["default"].item},v["default"].createElement(o["default"],{type:"paper-clip"}),v["default"].createElement("span",{onClick:e.handleSkip.bind(e,t.fileUrl)},t.fileName||"\u672a\u77e5\u540d\u5b57"))}):v["default"].createElement(r["default"],null)))}}]),t}(v.Component),d=s))||d);t["default"]=b},e6V8:function(e,t,a){e.exports={flex:"flex___2O1jk",cardTable:"cardTable___22Gmt"}},kePK:function(e,t,a){e.exports={container:"container___BUqiZ",left:"left___1iz3s",title:"title___1Ji35",info:"info___S1QPk",right:"right___2gnNi",options:"options___glvFi",statistics:"statistics___354iW",tabCard:"tabCard___3Wzqj"}},ulGV:function(e,t,a){e.exports={container:"container___3DdrF",item:"item___3cX7C"}}}]);