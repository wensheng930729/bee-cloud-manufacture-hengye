import { Component } from 'react'
import { Breadcrumb, Modal } from 'antd'
import styles from './index.less'
import withRouter from 'umi/withRouter'

const routerNames = {
  device: '设备管理',
  ammeter: '电表管理',
  electricityPrice: '电价管理',
  inspection: '设备巡检',
  deviceManage: '设备管理',
  weighDevice: '称重设备管理',
  plcManage: 'PLC设备管理',
  gateway: '网关管理',

  product: '产品管理',
  archives: '产品档案',
  test: '化验配置',
  specConfig: '规格配置',
  productCategory: '产品类别',

  quality: '质检管理',
  labProp: '化验属性',

  logistical: '物流管理',
  locationConfig: '地点配置',

  warehouse: '仓库管理',
  warehouseFile: '仓库档案',
  stock: '期初库存',

  statistics: '统计配置',
  materialCons: '原料吨耗配置',
  lossConfig: '原料损耗配置',
  boardConfig: '看板BI配置',
  reportConfig: '报表配置',

  crm: '客户及供应商管理',
  clientManage: '客户管理',
  supplierManage: '供应商管理',
  accountManage: '上下游账户管理',

  permission: '权限管理',
  userManage: '人员管理',
  roleManage: '角色管理'
}

@withRouter
export default class Index extends Component {
  render() {
    const { extra } = this.props;
    const { pathname } = this.props.location;
    let routerArr = pathname.split('/');
    routerArr.splice(0, 1);
    return (
      <div className={styles.container}>
        <Breadcrumb>
          <Breadcrumb.Item>工厂系统配置</Breadcrumb.Item>
          {routerArr.map((item, index) => {
            if (routerNames[item]) {
              return (
                <Breadcrumb.Item key={item}>
                  {routerNames[item]}
                </Breadcrumb.Item>
              )
            }
          })}
        </Breadcrumb>
        <div className={styles.extra}>{extra}</div>
      </div>
    )
  }
}
