import React, { PureComponent } from 'react'
import { Row, Col, Button, Card, Popconfirm, message, Form, InputNumber, DatePicker } from 'antd';
import { connect } from 'dva'
import styles from './index.less'
import * as dictionary from '@/constants/dictionary'
import { completeBuyContract, payForBuyContractBasic } from '../../../services/services'
import { MadalHOC } from '@/components'
import moment from 'moment'
import withRouter from "umi/withRouter"

@withRouter
@connect(({ purchase: { orderInfo } }) => ({ orderInfo }))
export default class Wraper extends PureComponent {
  state = {
    visible: false,
  }

  componentDidMount() {
    this.contractBusinessId = this.props.location.query.contractBusinessId;
  }

  //完成合同
  completeBuyContract(businessId) {
    completeBuyContract({ businessId }).then(res => {
      if (res.code === 1) {
        this.props.dispatch({
          type: "purchase/getBuyContractDetail",
          payload: this.contractBusinessId,
        })
      }
      message.info(res.message)
    });
  }

  //是否打开付款页面
  showPay(visible) {
    this.setState({
      visible
    })
  }

  //付款
  handleOk = ({ payAmount, payTime }) => {
    const params = { payAmount, payTime: moment(payTime).format('YYYY-MM-DD'), contractBusinessId: this.contractBusinessId }
    payForBuyContractBasic(params).then(res => {
      if (res.code === 1) {
        this.setState({
          visible: false
        })
        this.props.dispatch({
          type: "purchase/getBuyContractDetail",
          payload: this.contractBusinessId,
        })
      }
      message.info(res.message)
    })
  }

  render() {
    const { visible } = this.state;
    const { tabList, children, orderInfo: { contract = {} }, activeKey, onTabChange } = this.props;

    const newAProps = {
      visible,
      title: '付款',
      onOk: this.handleOk,
      onCancel: this.showPay.bind(this, false),
      width: 700,
      okText: '确定付款',
      cancelText: '取消',
    }
    return (
      <div className={styles.container}>
        <Row>
          <Col span={16} className={styles.left}>
            <div className={styles.title}> 合同编号：{contract.contractNum}</div>
            <Row className={styles.info}>
              <Col span={10}><span>　创建人：</span><span>{contract.creator}</span></Col>
              <Col span={10}><span>　供应商：</span><span>{contract.supplierName}</span></Col>
              <Col span={10}><span>签订日期：</span><span>{contract.signDate}</span></Col>
              <Col span={10}><span>　起始地：</span><span>{contract.originAddress}</span></Col>
              <Col span={10}><span>　联系人：</span><span>{contract.linkMan}</span></Col>
              <Col span={10}><span>采购方式：</span><span>{dictionary.purchaserMode[contract.purchaserMode || '0']}</span></Col>
            </Row>
          </Col>
          <div className={styles.right}>
            {contract.completed === 0 && activeKey === 'Detail' ? <div className={styles.options}>
              <Popconfirm
                title="确定完成该合同吗?"
                onConfirm={this.completeBuyContract.bind(this, contract.contractBusinessId)}
                okText="确定"
                cancelText="取消"
              ><Button>完成合同</Button></Popconfirm>
              <Button type='primary' onClick={this.showPay.bind(this, true)}>付款</Button>
            </div> : null}
            <div className={styles.statistics}>
              <div><span>状态</span><span>{dictionary.completed[contract.completed]}</span></div>
              <div><span>合同金额</span><span>¥ {contract.amount || 0}</span></div>
            </div>
          </div>
        </Row>
        <Card
          className={styles.tabCard}
          tabList={tabList}
          activeTabKey={activeKey}
          onTabChange={key => {
            onTabChange(key);
          }}
        >
          {children}
        </Card>

        <NewA
          {...newAProps}
        />

      </div>
    )
  }
}

class Pay extends PureComponent {
  render() {
    const { getFieldDecorator } = this.props.form;
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
      <Form.Item label="付款金额">
        {getFieldDecorator('payAmount', config)(<InputNumber />)}
      </Form.Item>
      <Form.Item label="付款时间">
        {getFieldDecorator('payTime', config)(<DatePicker />)}
      </Form.Item>
    </Form>
  }
}

// const NewPay = CustomFormHOC(Pay);

const NewA = MadalHOC(Form.create()(Pay))