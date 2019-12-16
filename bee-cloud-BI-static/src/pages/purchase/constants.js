export default {
  PRODUCT: {
    key: 'PRODUCT', name: '产品信息', columns: [
      {
        title: '产品名称',
        dataIndex: 'productName',
        key: 'productName',
      },
      {
        title: '质量要求',
        dataIndex: 'qualityRequirement',
        key: 'qualityRequirement',
      },
      {
        title: '单价',
        dataIndex: 'unitPrice',
        key: 'unitPrice',
      },
      {
        title: '数量',
        dataIndex: 'quantity',
        key: 'quantity',
      },
      {
        title: '金额',
        dataIndex: 'amount',
        key: 'amount',
      },
    ]
  },
  PAY: {
    key: 'PAY', name: '付款情况', columns: [
      {
        title: '付款日期',
        dataIndex: 'payTime',
        key: 'payTime',
      },
      {
        title: '支付方式',
        dataIndex: 'payType',
        key: 'payType',
        render: (text) => <span>现金</span>
      },
      {
        title: '付款金额',
        dataIndex: 'payAmount',
        key: 'payAmount',
      }
    ]
  },
  RECEIVE: {
    key: 'RECEIVE', name: '收货情况', columns: [
      {
        title: '收货日期',
        align: 'center',
        dataIndex: 'weighingTime',
        key: 'weighingTime',
      },
      {
        title: '车号',
        align: 'center',
        dataIndex: 'trainNumber',
        key: 'trainNumber',
      },
      {
        title: '净重',
        align: 'center',
        dataIndex: 'netWeight',
        key: 'netWeight',
      },
      {
        title: '司磅员',
        align: 'center',
        dataIndex: 'weightMan',
        key: 'weightMan',
      },
      {
        title: '质检结果',
        align: 'center',
        dataIndex: 'assayResult',
        key: 'assayResult',
        render: (text) => (<span>{text === 0 ? '不合格' : '合格'}</span>)
      },
      {
        title: '状态',
        dataIndex: 'handleType',
        align: 'center',
        key: 'handleType',
        render: (text) => (<span>{text === 0 ? '折价入库' : text === undefined ? "/" : '确认入库'}</span>)
      }
    ]
  }
}