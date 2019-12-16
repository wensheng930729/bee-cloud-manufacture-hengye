import React, { PureComponent } from 'react'
import { Card, Table, Dropdown, Menu, Icon, Popconfirm, Form, InputNumber, Modal, message } from 'antd';
import { connect } from 'dva'
import withRouter from "umi/withRouter"
import styles from './index.less'
import constants from '../../../constants'
import { MadalHOC } from '@/components'

import { bulkDiscountedProduct, bulkConfirmProduct, getCarSampleInfo, purchaserReAssay } from '../../../services/services'
const { confirm } = Modal


@withRouter
@connect(({ purchase: { tableDatas, orderInfo } }) => ({ tableDatas, orderInfo }))
export default class Index extends PureComponent {
  state = {
    sampleInfo: {},//化验结果数据
    showReslut: false,//化验结果
    discountInfo: {},//折价入库数据
    showDiscount: false//折价入库
  }

  componentDidMount() {
    this.contractBusinessId = this.props.location.query.contractBusinessId;
    this.getData({});
  }

  //获取列表数据
  getData({ currentPage = 1 }) {
    const { dispatch } = this.props;
    dispatch({
      type: "purchase/getCarList",
      payload: { contractBusinessId: this.contractBusinessId, currentPage, pageSize: 5 }
    })
  }

  //翻页
  onChange({ current }) {
    this.getData({ currentPage: current })
  }

  // 查看化验结果
  showResult(machineId) {
    getCarSampleInfo({ machineId }).then(res => {
      if (res.code === 1) {
        this.setState({
          sampleInfo: res.object,
          showReslut: true
        })
      }
    })
  }

  // 再次化验
  purchaserReAssay(machineId) {
    purchaserReAssay({ machineId }).then(res => {
      if (res.code === 1) {
        this.getData({});
        message.success(res.message);
      } else {
        message.error(res.message);
      }
    })
  }

  //更多操作
  moreOptionClick(row, { key }) {
    const self = this;
    switch (key) {
      case '1':
        confirm({
          title: '该车次入库后无法修改',
          content: '是否继续入库？',
          okText: '确定',
          cancelText: '取消',
          onOk() {
            self.bulkConfirmProduct(row.machineId)
          },
          onCancel() {
          },
        });
        break;
      case '2':
        this.setState({
          discountInfo: row,
          showDiscount: true
        })
        break;
      default:
        break;
    }
  }

  //合同确认入库
  bulkConfirmProduct = (machineId) => {
    bulkConfirmProduct([{ machineId }]).then(res => {
      res.code === 1 ? message.success(res.message, 1, () => this.getData({})) : message.error(res.message);
    })
  }

  //折价入库
  bulkDiscountedProduct = (info, values) => {
    const params = { ...values, machineIds: [info.machineId] }
    bulkDiscountedProduct(params).then(res => {
      if (res.code === 1) {
        this.getData({});
        this.setState({
          showDiscount: false
        })
      }
      message.info(res.message);
    })
  }

  render() {
    const { showReslut, sampleInfo, showDiscount, discountInfo } = this.state;
    const { tableDatas, orderInfo: { contract = {} } } = this.props;
    let { data = [], page = {} } = tableDatas[constants.RECEIVE.key];
    const pagination = { current: page.currentPage || 1, total: page.totalRecords || 0, pageSize: page.pageSize || 6 };

    //化验结果弹出层属性
    const ResultMadalProps = {
      visible: showReslut,
      title: '查看化验结果',
      onOk: () => this.setState({ showReslut: false }),
      onCancel: () => this.setState({ showReslut: false }),
      width: 700,
      okText: '确定',
      cancelText: '取消',
      ComProps: { data: sampleInfo }
    }

    //折价入库弹出层属性
    const DiscountMadalProps = {
      visible: showDiscount,
      title: '折价入库',
      onOk: this.bulkDiscountedProduct.bind(this, discountInfo),
      onCancel: () => this.setState({ showDiscount: false }),
      width: 700,
      okText: '确定',
      cancelText: '取消',
      ComProps: { data: { ...discountInfo, ...contract } }
    }
    //表头
    let columns = [...constants.RECEIVE.columns];
    columns.push({
      title: '操作',
      align: 'center',
      dataIndex: '_option',
      key: '_option',
      render: (text, row) => (<><a onClick={this.showResult.bind(this, row.machineId)}>查看化验结果</a>　&nbsp;
        <Popconfirm
          title="确定再次化验该样品吗?"
          onConfirm={this.purchaserReAssay.bind(this, row.machineId)}
          okText="确定"
          cancelText="取消"
        >
          <a>再次化验</a></Popconfirm>　&nbsp;
        {
          row.handleType === undefined ? <Dropdown overlay={menu(row)}>
            <a className="ant-dropdown-link">
              更多操作 <Icon type="down" />
            </a>
          </Dropdown> : null
        } </>),
    })

    //更多操作
    const menu = (row) => (
      <Menu onClick={this.moreOptionClick.bind(this, row)}>
        <Menu.Item key={1}>
          <a target="_blank">
            确认入库
          </a>
        </Menu.Item>
        <Menu.Item key={2}>
          <a target="_blank">
            折价入库
          </a>
        </Menu.Item>
        {/* <Menu.Item key={3}>
          <a target="_blank">
            退货
          </a>
        </Menu.Item> */}
      </Menu>
    );
    return (
      <>
        <CardTable columns={columns} data={data} pagination={pagination} name={constants.RECEIVE.name} onChange={this.onChange.bind(this)} />
        <ResultWraper  {...ResultMadalProps} />
        <DiscountWraper {...DiscountMadalProps} />
      </>
    )
  }
}


//产品信息
const CardTable = ({ name, columns, data, pagination, onChange }) => {
  return (
    <Card
      title={name}
      className={styles.cardTable}
    >
      <Table columns={columns} dataSource={data} pagination={pagination} onChange={onChange} />
    </Card>
  )
}

const Result = ({ data = {} }) => {
  return <div className={styles.result}>
    <section>
      <span>合同编号：{data.contractNum}</span>
      <span>产品名称：{data.productName}</span>
      <span>质量要求：{data.qualityRequirement}</span>
    </section>
    <>
      {data.sampleResultDTOs && data.sampleResultDTOs.length ? data.sampleResultDTOs.map(item => <section>
        <span>样品编号：{item.sampleCode}</span>
        <span>　　规格：{item.productSpecName}</span>
        <span>取样时间：{item.createTime}</span>
        <span>化验时间：{item.assayTime}</span>
        <span>化验结果：<ul>{item.assays && item.assays.length ? item.assays.map(_item => (<><li>
          <div>{_item.assayItem}</div><div>{_item.assayValue + '' + _item.unitString}</div></li></>)) : null} </ul> </span>
      </section>) : null}
    </>
  </div>
}

const ResultWraper = MadalHOC(Result)

class Discount extends PureComponent {
  render() {
    const { data, form: { getFieldDecorator } } = this.props;
    const formItemLayout = {
      labelCol: {
        xs: { span: 24 },
        sm: { span: 8 },
      },
      wrapperCol: {
        xs: { span: 24 },
        sm: { span: 16 },
      },
    };
    const config = {
      rules: [{ required: true }]
    }
    return <Form {...formItemLayout}>
      <Form.Item label="供应商">
        {data.supplierName}
      </Form.Item>
      <Form.Item label="产品名称">
        {data.productName}
      </Form.Item>
      <Form.Item label="净重">
        {data.cargoWeight || 0}吨
      </Form.Item>
      <Form.Item label="折后单价">
        {getFieldDecorator('discountedPrice', config)(<InputNumber />)}
      </Form.Item>
    </Form>
  }
}

const DiscountWraper = MadalHOC(Form.create()(Discount))