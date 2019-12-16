import React, { Component } from 'react'
import { Row, Col, Button, Card, Table, Input, message, InputNumber, DatePicker, Empty } from 'antd';
import { connect } from 'dva';
import styles from './index.less';
import router from 'umi/router';
import withRouter from "umi/withRouter";
import { getContractSettleInfo, saveContractSettleInfo, sureContractSettle, updateSettleAmountBuyContract, getAuth } from '../../../services/services';
import moment from 'moment';

const formatter = "YYYY-MM-DD";

@withRouter
@connect(({ purchase: { orderInfo } }) => ({ orderInfo }))
export default class Settlement extends Component {
  state = {
    // 供销结算详情
    settleStatus: 0,
    settlementData: [],
    editIndex: null,
    editObj: {
      unitPriceSettlement: null,
      amountSettlement: null,
    },
    auth: []
  }

  componentDidMount() {
    this.getSettle();
    getAuth().then(res => {
      if (res.code === 1) {
        this.setState({
          auth: res.object
        })
      }
    })
  }

  getSettle = () => {
    const { contractBusinessId } = this.props.location.query;

    getContractSettleInfo(contractBusinessId).then(res => {
      if (res.code === 1) {
        let newArray = [];
        if (res.object.settleDTOS) {
          res.object.settleDTOS.forEach(item => {
            newArray.push({
              ...item,
              edit: false
            })
          })
        }
        this.setState({
          settlementData: newArray,
          settleStatus: res.object.settleStatus
        })
      }
    })
  }

  updateSettle = (contractSettlementBusinessId) => {
    const { dispatch } = this.props;
    const { contractBusinessId } = this.props.location.query;
    const { editObj: { unitPriceSettlement, amountSettlement } } = this.state;

    if (!contractSettlementBusinessId) {
      return message.warning("结算单数据异常");
    }

    if ((!unitPriceSettlement && unitPriceSettlement !== 0) || !amountSettlement && amountSettlement !== 0) {
      return message.warning("请输入结算单价和结算金额再重试！");
    }

    updateSettleAmountBuyContract({
      contractSettlementBusinessId,
      unitPriceSettlement,
      amountSettlement
    }).then(res => {
      if (res.code === 1) {
        message.success("确认成功");
        this.setState({
          editIndex: null,
          unitPriceSettlement: null,
          amountSettlement: null,
        })
        dispatch({
          type: "purchase/getBuyContractDetail",
          payload: contractBusinessId,
        })
      } else {
        message.error("确认失败：" + res.message);
      }
    })
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
    let { settleTime, weightReceive, weightSettle, waterContentSettle } = settlementData[index];

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

    saveContractSettleInfo({
      ...settlementData[index],
      contractBusinessId
    }).then(res => {
      if (res.code === 1) {
        message.success("确认成功");
        dispatch({
          type: "purchase/getBuyContractDetail",
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

  completeSettle = () => {
    const { dispatch } = this.props;
    const { contractBusinessId } = this.props.location.query;

    sureContractSettle({ contractBusinessId }).then(res => {
      if (res.code === 1) {
        message.success("完成结算成功");
        this.getSettle();
      } else {
        message.error(res.message);
      }
    })
  }

  render() {
    const { orderInfo } = this.props;
    const { editIndex, settlementData, settleStatus, auth } = this.state;

    const column = [
      {
        title: '结算时间',
        dataIndex: 'settleTime',
        key: 'settleTime',
        width: '14%'
      }, {
        title: '收货数量',
        dataIndex: 'weightReceive',
        key: 'weightReceive',
        width: '14%'
      }, {
        title: '结算数量',
        dataIndex: 'weightSettle',
        key: 'weightSettle',
        width: '14%'
      }, {
        title: '结算水分',
        dataIndex: 'waterContentSettle',
        key: 'waterContentSettle',
        width: '14%'
      }, {
        title: '结算单价',
        dataIndex: 'unitPriceSettlement',
        key: 'unitPriceSettlement',
        render: (text, row, index) => editIndex === index ?
          <Input
            defaultValue={text}
            onChange={(e) => this.setState({
              editObj: {
                ...this.state.editObj,
                unitPriceSettlement: Number(e.target.value)
              }
            })}
          />
          : text,
        width: '15%'
      }, {
        title: '结算金额',
        dataIndex: 'amountSettlement',
        key: 'amountSettlement',
        render: (text, row, index) => editIndex === index ?
          <Input
            defaultValue={text}
            onChange={(e) => this.setState({
              editObj: {
                ...this.state.editObj,
                amountSettlement: Number(e.target.value)
              }
            })}
          />
          : text,
        width: '15%'
      }, {
        title: '操作',
        dataIndex: 'actions',
        key: 'actions',
        // 结算状态为1时可编辑
        render: (text, row, index) => !orderInfo.completed && row.settlementStatus === 1 ? (
          editIndex !== index ?
            <div className={styles.actions}>
              <span onClick={() => this.setState({ editIndex: index })}>编辑</span>
            </div>
            :
            <div className={styles.actions}>
              <span onClick={this.updateSettle.bind(this, row.contractSettlementBusinessId)}>确认</span>
              <span onClick={() => this.setState({ editIndex: null })}>取消</span>
            </div>
        ) : <span style={{ cursor: 'default', color: 'rgba(0, 0, 0, .25)' }}>已确认</span>,
        width: '14%'
      },
    ]

    const column_two = [
      {
        title: '结算时间',
        dataIndex: 'settleTime',
        key: 'settleTime',
        render: (text, row, index) => !settleStatus && row.edit ?
          <DatePicker
            style={{ width: '80%' }}
            allowClear={false}
            value={text ? moment(text) : null}
            format={formatter}
            showTime={false}
            onChange={(date, dateString) => this.onChange(index, 'settleTime', dateString)}
          />
          : text,
        width: '20%'
      }, {
        title: '收货数量',
        dataIndex: 'weightReceive',
        key: 'weightReceive',
        render: (text, row, index) => !settleStatus && row.edit ?
          <InputNumber value={text} style={{ width: '80%' }} onChange={(value) => this.onChange(index, 'weightReceive', value)} />
          : text,
        width: '20%'
      }, {
        title: '结算数量',
        dataIndex: 'weightSettle',
        key: 'weightSettle',
        render: (text, row, index) => !settleStatus && row.edit ?
          <InputNumber value={text} style={{ width: '80%' }} onChange={(value) => this.onChange(index, 'weightSettle', value)} />
          : text,
        width: '20%'
      }, {
        title: '结算水分',
        dataIndex: 'waterContentSettle',
        key: 'waterContentSettle',
        render: (text, row, index) => !settleStatus && row.edit ?
          <InputNumber value={text} style={{ width: '80%' }} onChange={(value) => this.onChange(index, 'waterContentSettle', value)} />
          : text,
        width: '20%'
      }, {
        title: '操作',
        dataIndex: 'actions',
        key: 'actions',
        render: (text, row, index) => !settleStatus && row.edit ? (
          <div className={styles.actions}>
            <span onClick={this.saveSettleInfo.bind(this, index)}>保存</span>
            <span onClick={this.deleteSettle.bind(this, index)}>删除</span>
          </div>
        ) : (
            <div className={styles.actions}>
              <span style={{ cursor: 'default', color: 'rgba(0, 0, 0, .25)' }}>已确认</span>
            </div>
          ),
        width: '20%'
      },
    ]

    return (
      <div className={styles.container}>
        {
          auth.length === 0 ? <Empty description="您暂无相应权限" /> : (
            <Row>
              
              {
                auth.includes("maf_settlement_auth") ? (
                  <Col span={24}>
                    <Card title="供销结算单" extra={!settleStatus ? <Button onClick={this.completeSettle.bind(this)}>完成供销结算</Button> : null}>
                      <Table
                        rowKey={(row, index) => index}
                        columns={column_two}
                        dataSource={settlementData}
                        pagination={false}
                      />
                      {
                        !settleStatus ? <Button onClick={this.addSettle.bind(this)} icon="plus" block style={{ marginTop: 12 }}>新增结算</Button> : null
                      }
                    </Card>
                  </Col>
                ) : null
              }
              {
                auth.includes("maf_purchase_auth") ? (
                  <Col span={24}>
                    <Card title="采购结算单">
                      <Table
                        rowKey={(row, index) => index}
                        columns={column}
                        dataSource={orderInfo && orderInfo.settle && orderInfo.settle.data ? orderInfo.settle.data : []}
                        pagination={false}
                      />
                    </Card>
                  </Col>
                ) : null
              }

            </Row>
          )
        }
      </div>
    )
  }
}