import React, { Component } from 'react'
import { Card, Table } from 'antd';
import { connect } from 'dva'
import styles from './index.less'
import constants from '../../../constants'

@connect(({ sale: { orderInfo, tableDatas } }) => ({ orderInfo, tableDatas }))
export default class Wraper extends Component {

  render() {
    const { orderInfo: { contract = {} }, tableDatas } = this.props;
    return (
      <>
        <Card
          title={'物流信息'}
        >
          <div className={styles.flex}>
            <span>合同数量：{contract.quantity || '0'}吨</span> <span>已发量：{contract.issuedVolume || '0'}吨</span>
            <span>在途量：{contract.trafficVolume || '0'}吨</span> <span>已到量：{contract.arrivalVolume || '0'}吨</span>
            <span>未发量：{contract.undeliveredVolume || '0'}吨</span></div>
        </Card>
        <CardTable columns={constants.PRODUCT.columns} data={tableDatas[constants.PRODUCT.key]} name={constants.PRODUCT.name} />
        <CardTable columns={constants.PAY.columns} data={tableDatas[constants.PAY.key]} name={constants.PAY.name} />
      </>
    )
  }
}

Wraper.defaultProps = {
  activeKey: '',
  tabList: [],
  onTabChange: () => null,
}


//产品信息
const CardTable = ({ name, columns, data }) => {
  return (
    <Card
      title={name}
      className={styles.cardTable}
    >
      <Table pagination={false} columns={columns} dataSource={data} />
    </Card>
  )
}