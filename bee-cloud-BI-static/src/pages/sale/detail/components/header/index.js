import React, { PureComponent } from 'react'
import { Row, Col, Button, Card, Popconfirm, message, Form, InputNumber, DatePicker } from 'antd';
import { connect } from 'dva'
import withRouter from "umi/withRouter"
import styles from './index.less'
import * as dictionary from '@/constants/dictionary'
import { completeSaleContract, receiveForSaleContract } from '../../../services/services'
import { MadalHOC } from '@/components'
import moment from 'moment'
@withRouter
@connect(({ sale: { orderInfo } }) => ({ orderInfo }))
export default class Wraper extends PureComponent {
  state = {
    visible: false,
  }

  componentDidMount() {
    this.contractBusinessId = this.props.location.query.contractBusinessId;
  }

  //完成合同
  completeSaleContract(businessId) {
    completeSaleContract({ businessId }).then(res => {
      if (res.code === 1) {
        this.props.dispatch({
          type: "sale/getSaleContractDetail",
          payload: this.contractBusinessId,
        })
      }
      message.info(res.message)
    });
  }

  //是否打开收款页面
  showPay(visible) {
    this.setState({
      visible
    })
  }

  //收款
  handleOk = ({ paymentAmount, receiveTime }) => {
    const params = { paymentAmount, receiveTime: moment(receiveTime).format('YYYY-MM-DD'), contractBusinessId: this.contractBusinessId }
    receiveForSaleContract(params).then(res => {
      if (res.code === 1) {
        this.setState({
          visible: false
        })
        this.props.dispatch({
          type: "sale/getSaleContractDetail",
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
      title: '收款',
      onOk: this.handleOk,
      onCancel: this.showPay.bind(this, false),
      okText: '确定收款',
      cancelText: '取消',
    }
    return (
      <div className={styles.container}>
        <Row>
          <Col span={16} className={styles.left}>
            <div className={styles.title}> 合同编号：{contract.contractNum}</div>
            <Row className={styles.info}>
              <Col span={10}><span>　创建人：</span><span>{contract.creator}</span></Col>
              <Col span={10}><span>　　客户：</span><span>{contract.customerName}</span></Col>
              <Col span={10}><span>签订日期：</span><span>{contract.signDate}</span></Col>
              <Col span={10}><span>　到达地：</span><span>{contract.arrivalAddress}</span></Col>
              <Col span={10}><span>　联系人：</span><span>{contract.linkMan}</span></Col>
              <Col span={10}><span>销售方式：</span><span>{dictionary.saleMode[contract.saleMode || '0']}</span></Col>
            </Row>
          </Col>
          <div className={styles.right}>
            {contract.completed === 0 && activeKey === 'Detail' ? <div className={styles.options}>
              <Popconfirm
                title="确定完成该合同吗?"
                onConfirm={this.completeSaleContract.bind(this, contract.contractBusinessId)}
                okText="确定"
                cancelText="取消"
              ><Button>完成合同</Button></Popconfirm>
              <Button type='primary' onClick={this.showPay.bind(this, true)}>收款</Button>
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
      <Form.Item label="收款金额">
        {getFieldDecorator('paymentAmount', config)(<InputNumber />)}
      </Form.Item>
      <Form.Item label="收款时间">
        {getFieldDecorator('receiveTime', config)(<DatePicker />)}
      </Form.Item>
    </Form>
  }
}

// const NewPay = CustomFormHOC(Pay);

const NewA = MadalHOC(Form.create()(Pay))