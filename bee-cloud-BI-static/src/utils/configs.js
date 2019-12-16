
const roles = {
  '1': {
    dashboard: [
      // { name: 'Welcome', styles: {} },
      { name: 'Finance', styles: {} },
      { name: 'Bill', styles: {} }
    ]
  },
  '2': {
    dashboard: [
      // { name: 'Welcome', styles: {} },
      { name: 'Finance', styles: {} },
      { name: 'Bill', styles: {} }
    ]
  },
  '3': {
    dashboard: []
  },
  '4': {
    dashboard: [
      { name: 'MiniPanels', styles: {} },
      { name: 'CargoRight', styles: {} },
      { name: 'AmountSituation', styles: { height: 486 }, span: 8 },
      { name: 'RiskRatio', styles: { height: 486 }, span: 16 },
      { name: 'Amount', styles: {} },
      { name: 'Finance', styles: {} },
      { name: 'Bill', styles: {} },
      { name: 'Risk', styles: {} },
    ]
  },
  '5': {
    dashboard: [
      { name: 'MiniPanels', styles: {} },
      { name: 'CargoRight', styles: {} },
      { name: 'AmountSituation', styles: { height: 486 }, span: 8 },
      { name: 'RiskRatio', styles: { height: 486 }, span: 16 },
      { name: 'Amount', styles: {} },
      { name: 'Finance', styles: {} },
      { name: 'Bill', styles: {} },
      { name: 'Risk', styles: {} },

    ]
  },
  '6': {
    dashboard: [
      { name: 'MiniPanels', styles: {} },
      { name: 'CargoRight', styles: {} },
      { name: 'AmountSituation', styles: { height: 486 }, span: 8 },
      { name: 'RiskRatio', styles: { height: 486 }, span: 16 },
      { name: 'Amount', styles: {} },
      { name: 'Finance', styles: {} },
      { name: 'Bill', styles: {} },
      { name: 'Risk', styles: {} },
    ]
  },
  '7': {
    dashboard: [
      { name: 'MiniPanels', styles: {} },
      { name: 'CargoRight', styles: {} },
      { name: 'AmountSituation', styles: { height: 486 }, span: 8 },
      { name: 'RiskRatio', styles: { height: 486 }, span: 16 },
      { name: 'Amount', styles: {} },
      { name: 'Finance', styles: {} },
      { name: 'Bill', styles: {} },
      { name: 'Risk', styles: {} },
    ]
  },
  '8': {
    dashboard: [
      { name: 'MiniPanels', styles: {} },
      { name: 'CargoRight', styles: {} },
      { name: 'AmountSituation', styles: { height: 486 }, span: 8 },
      { name: 'RiskRatio', styles: { height: 486 }, span: 16 },
      { name: 'Amount', styles: {} },
      { name: 'Finance', styles: {} },
      { name: 'Bill', styles: {} },
      { name: 'Risk', styles: {} },
    ]
  },
  '9': {
    dashboard: []
  },
  '10': {
    dashboard: [
      { name: 'MiniPanels', styles: {} },
      { name: 'CargoRight', styles: {} },
      { name: 'AmountSituation', styles: { height: 486 }, span: 8 },
      { name: 'RiskRatio', styles: { height: 486 }, span: 16 },
      { name: 'Amount', styles: {} },
      { name: 'Finance', styles: {} },
      { name: 'Bill', styles: {} },
      { name: 'Risk', styles: {} },
    ]
  },
  '11': {
    dashboard: [
      { name: 'MiniPanels', styles: {} },
      { name: 'CargoRight', styles: {} },
      { name: 'AmountSituation', styles: { height: 486 }, span: 8 },
      { name: 'RiskRatio', styles: { height: 486 }, span: 16 },
      { name: 'Amount', styles: {} },
      { name: 'Finance', styles: {} },
      { name: 'Bill', styles: {} },
      { name: 'Risk', styles: {} },
    ]
  },
  '12': {
    dashboard: [
      { name: 'MiniPanels', styles: {} },
      { name: 'CargoRight', styles: {} },
      { name: 'AmountSituation', styles: { height: 486 }, span: 8 },
      { name: 'RiskRatio', styles: { height: 486 }, span: 16 },
      { name: 'Amount', styles: {} },
      { name: 'Finance', styles: {} },
      { name: 'Bill', styles: {} },
      { name: 'Risk', styles: {} },
    ]
  },
  '13': {
    dashboard: []
  },
  '14': {
    dashboard: [
      { name: 'MiniPanels', styles: {} },
      { name: 'CargoRight', styles: {} },
      { name: 'AmountSituation', styles: { height: 486 }, span: 8 },
      { name: 'RiskRatio', styles: { height: 486 }, span: 16 },
      { name: 'Amount', styles: {} },
      { name: 'Finance', styles: {} },
      { name: 'Bill', styles: {} },
      { name: 'Risk', styles: {} },
    ]
  },
  '15': {
    dashboard: [
      { name: 'MiniPanels', styles: {} },
      { name: 'CargoRight', styles: {} },
      { name: 'AmountSituation', styles: { height: 486 }, span: 8 },
      { name: 'RiskRatio', styles: { height: 486 }, span: 16 },
      { name: 'Amount', styles: {} },
      { name: 'Finance', styles: {} },
      { name: 'Bill', styles: {} },
      { name: 'Risk', styles: {} },
    ]
  },
  '16': {
    dashboard: [
      { name: 'MiniPanels', styles: {} },
      { name: 'CargoRight', styles: {} },
      { name: 'AmountSituation', styles: { height: 486 }, span: 8 },
      { name: 'RiskRatio', styles: { height: 486 }, span: 16 },
      { name: 'Amount', styles: {} },
      { name: 'Finance', styles: {} },
      { name: 'Bill', styles: {} },
      { name: 'Risk', styles: {} },
    ]
  },
  // '-1': { //企业看板
  //   dashboard: [
  //     { name: 'Welcome', styles: {} },
  //     { name: 'Finance', styles: {} },
  //     { name: 'Bill', styles: {} },
  //     { name: 'TotalProfit', styles: { height: 486 }, span: 8 },
  //     { name: 'RiskRatio', styles: { height: 486 }, span: 16 },
  //     { name: 'Risk', styles: {} }
  //   ]
  // },
}
//业务类型
const businessModes = [{ name: '委托采购', key: '0', abbr: 'buy' }, { name: '委托销售', key: '1', abbr: 'sale' },
{ name: '金融仓储', key: '2', abbr: 'storage' }, { name: '大企业委托采购', key: '4', abbr: 'largeBuy' }]

//看板货权管理显示
const cargoRight = {
  buy: [
    {
      icon: '供', title: '供应商',
      statistics: [
        // { key: 'shippedQuantity', label: '已发货数量', unit: '吨' },
        { key: 'outShippedQuantity', label: '未发货数量', unit: '吨' }
      ]
    }, {
      icon: '资', title: '合作伙伴',
      statistics: [
        { key: 'extractNot', label: '未提数量', unit: '吨' }
      ]
    }, {
      icon: '委', title: '委托企业',
      statistics: [{ key: 'extractAlready', label: '已提数量', unit: '吨' }, { key: 'accountQuantity', label: '已结算数量', unit: '吨' }
      ]
    }
  ],
  largeBuy: [
    {
      icon: '委', title: '委托企业',
      statistics: [
        // { key: 'shippedQuantity', label: '已发货数量', unit: '吨' },
        { key: 'outShippedQuantity', label: '未发货数量', unit: '吨' }
      ]
    }, {
      icon: '采', title: '采购商',
      statistics: [
        { key: 'arrivalAccountQuantity', label: '已到数量', unit: '吨' }, { key: 'accountQuantity', label: '已结算数量', unit: '吨' }
      ]
    }
  ],
  sale: [
    {
      icon: '委', title: '委托企业',
      statistics: [
        { key: 'outShippedQuantity', label: '未发货数量', unit: '吨' }
      ]
    }, {
      icon: '购', title: '购货商',
      statistics: [
        // { key: 'shippedQuantity', label: '已发货数量', unit: '吨' },
        { key: 'arrivalAccountQuantity', label: '已到数量', unit: '吨' }, { key: 'accountQuantity', label: '已结算数量', unit: '吨' }
      ]
    }
  ],
}

export { roles, businessModes, cargoRight }
//         entru (1, "委托方"),
//         core_enterprise(2, "核心企业"),
//         forwarding_company(3, "货代公司"),
//         counterman(4, "业务员"),
//         department_manager(5, "部门经理"),
//         risk_commissioner(6, "风控专员"),
//         business_risk (7, "业务风控"),
//         risk_manager (8, "风控负责人"),
//         data_steward(9, "数据专员"),
//         settlement_specialist(10, "结算专员"),
//         settlement_reviewer(11, "结算复核人"),
//         settlement_officer(12, "结算负责人"),
//         law_work(13, "法务"),
//         caucus_member(14, "决策委员会成员"),
//         capital(15, "资方"),
//         super_administrator(16, "超级管理员");
//         other(999, "其它用户模块");
