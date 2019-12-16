import React, { Component } from 'react'
import { Row, Col, Button, Card, Table, message, InputNumber, DatePicker } from 'antd';
import { connect } from 'dva';
import styles from './index.less';
import withRouter from "umi/withRouter";
import { saveContractSettleInfo } from '../../../services/services';
import moment from 'moment';

const formatter = "YYYY-MM-DD";

@withRouter
@connect(({ sale: { orderInfo } }) => ({ orderInfo }))
export default class Settlement extends Component {
  state = {
    completed: 0,
    settlementData: []
  }

  componentDidMount() {
    const { orderInfo } = this.props;
    if (orderInfo && orderInfo.contract) {
      this.setState({ completed: orderInfo.contract.completed })
    }

    if (orderInfo && orderInfo.settle && orderInfo.settle.data) {
      this.setState({ settlementData: orderInfo.settle.data })
    }
  }

  onChange = (index, key, value) => {
    let { settlementData } = this.state;
    settlementData[index][key] = value;
    this.setState({ settlementData })
  }

  addSettle = () => {
    let { settlementData } = this.state;
    settlementData.push({
      settleTime: null,
      weightReceive: null,
      weightSettle: null,
      waterContentSettle: null,
      unitPriceSettlement: null,
      amountSettlement: null,
      edit: true
    })
    this.setState({ settlementData })
  }

  deleteSettle = (index) => {
    let { settlementData } = this.state;
    settlementData.splice(index, 1);
    this.setState({ settlementData })
  }

  saveSettleInfo = (index) => {
    const { dispatch } = this.props;
    const { contractBusinessId } = this.props.location.query;
    let { settlementData } = this.state;
    let { settleTime, weightReceive, weightSettle, waterContentSettle, unitPriceSettlement, amountSettlement } = settlementData[index];

    if (!settleTime) {
      return message.error("请选择结算时间");
    }
    if (!weightReceive && weightReceive !== 0) {
      return message.error("请输入收货数量");
    }
    if (!weightSettle && weightSettle !== 0) {
      return message.error("请输入结算数量");
    }
    if (!waterContentSettle && waterContentSettle !== 0) {
      return message.error("请输入结算水分");
    }
    if (!unitPriceSettlement && unitPriceSettlement !== 0) {
      return message.error("请输入结算单价");
    }
    if (!amountSettlement && amountSettlement !== 0) {
      return message.error("请输入结算金额");
    }

    saveContractSettleInfo({
      ...settlementData[index],
      contractBusinessId
    }).then(res => {
      if (res.code === 1) {
        message.success("确认成功");
        dispatch({
          type: "sale/getSaleContractDetail",
          payload: contractBusinessId,
        })
        settlementData[index]["edit"] = false;
        this.setState({
          settlementData
        })
      } else {
        message.error(res.message);
      }
    })
  }

  render() {
    const { settlementData, completed } = this.state;

    const column = [
      {
        title: '结算时间',
        dataIndex: 'settleTime',
        key: 'settleTime',
        render: (text, row, index) => !completed && row.edit ?
          <DatePicker
            style={{ width: '80%' }}
            allowClear={false}
            value={text ? moment(text) : null}
            format={formatter}
            showTime={false}
            onChange={(date, dateString) => this.onChange(index, 'settleTime', dateString)}
          />
          : text,
        width: '15%'
      }, {
        title: '收货数量',
        dataIndex: 'weightReceive',
        key: 'weightReceive',
        render: (text, row, index) => !completed && row.edit ?
          <InputNumber value={text} style={{ width: '80%' }} onChange={(value) => this.onChange(index, 'weightReceive', value)} />
          : text,
        width: '15%'
      }, {
        title: '结算数量',
        dataIndex: 'weightSettle',
        key: 'weightSettle',
        render: (text, row, index) => !completed && row.edit ?
          <InputNumber value={text} style={{ width: '80%' }} onChange={(value) => this.onChange(index, 'weightSettle', value)} />
          : text,
        width: '15%'
      }, {
        title: '结算水分',
        dataIndex: 'waterContentSettle',
        key: 'waterContentSettle',
        render: (text, row, index) => !completed && row.edit ?
          <InputNumber value={text} style={{ width: '80%' }} onChange={(value) => this.onChange(index, 'waterContentSettle', value)} />
          : text,
        width: '15%'
      }, {
        title: '结算单价',
        dataIndex: 'unitPriceSettlement',
        key: 'unitPriceSettlement',
        render: (text, row, index) => !completed && row.edit ?
          <InputNumber value={text} style={{ width: '80%' }} onChange={(value) => this.onChange(index, 'unitPriceSettlement', value)} />
          : text,
        width: '15%'
      }, {
        title: '结算金额',
        dataIndex: 'amountSettlement',
        key: 'amountSettlement',
        render: (text, row, index) => !completed && row.edit ?
          <InputNumber value={text} style={{ width: '80%' }} onChange={(value) => this.onChange(index, 'amountSettlement', value)} />
          : text,
        width: '15%'
      }, {
        title: '操作',
        dataIndex: 'actions',
        key: 'actions',
        render: (text, row, index) => !completed && row.edit ? (
          <div className={styles.actions}>
            <span onClick={this.saveSettleInfo.bind(this, index)}>保存</span>
            <span onClick={this.deleteSettle.bind(this, index)}>删除</span>
          </div>
        ) : (
            <div className={styles.actions}>
              <span style={{ cursor: 'default', color: 'rgba(0, 0, 0, .25)' }}>已确认</span>
            </div>
          ),
        width: '10%'
      },
    ]

    return (
      <div className={styles.container}>
        <Row>
          <Col span={24}>
            <Card title="结算单">
              <Table
                rowKey={(row, index) => index}
                columns={column}
                dataSource={settlementData}
                pagination={false}
              />
              {
                !completed ? <Button onClick={this.addSettle.bind(this)} icon="plus" block style={{ marginTop: 12 }}>新增结算</Button> : null
              }
            </Card>
          </Col>
        </Row>
      </div>
    )
  }
}
