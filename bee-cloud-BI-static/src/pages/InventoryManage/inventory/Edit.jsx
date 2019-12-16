import React, { Component } from 'react';
import { Form, Row, Col, Card, Steps, Button, message } from 'antd';
import { connect } from 'dva';
import router from 'umi/router';
import { Step2, Step1 } from './Steps';
import styles from './index.less';

@connect(({ InventoryModel: { typeDesc, types, step1Data, step2Data } }) => ({
  typeDesc,
  step1Data,
  types,
  step2Data,
}))
class Edit extends Component {
  constructor(props) {
    super(props);
    this.state = {
      current: 0,
    };
  }

  createInventoryOrder() {
    const { step1Data, dispatch } = this.props;
    dispatch({
      type: 'InventoryModel/createInventoryOrder',
      payload: { ...step1Data },
      callback: () => this.next(),
    });
  }

  saveInventoryInfo() {
    try {
      _czc1.push(['_trackEvent', '盘点单', '新增', '', '', '']);
    } catch (error) {}
    const { step2Data, dispatch } = this.props;
    dispatch({
      type: 'InventoryModel/saveInventoryInfo',
      payload: { ...step2Data },
      callback: res => {
        if (res.code === 1) {
          message.success(res.message, 1, () => {
            router.push('/inventoryManage/inventory');
          });
        } else {
          message.error(res.message);
        }
      },
    });
  }

  next() {
    const current = this.state.current + 1;
    this.setState({ current });
  }

  prev() {
    const current = this.state.current - 1;
    this.setState({ current });
  }

  render() {
    const { current } = this.state;

    const { typeDesc, types, step1Data, step2Data, dispatch } = this.props;
    const steps = [
      {
        title: 'First',
        content: <Step1 datas={step1Data} types={types} typeDesc={typeDesc} dispatch={dispatch} />,
      },
      {
        title: 'Second',
        content: <Step2 datas={step2Data} dispatch={dispatch} />,
      },
    ];

    return (
      <Card>
        <div className={styles.stepsContent}>{steps[current].content}</div>
        <div className={styles.stepsAction}>
          {current < steps.length - 1 && (
            <Button type="primary" onClick={this.createInventoryOrder.bind(this)}>
              下一步
            </Button>
          )}

          {current > 0 && (
            <Button style={{ marginLeft: 8 }} onClick={() => this.prev()}>
              上一步
            </Button>
          )}
          {current === steps.length - 1 && (
            <Button type="primary" onClick={this.saveInventoryInfo.bind(this)}>
              保存
            </Button>
          )}
        </div>
      </Card>
    );
  }
}

export default Edit;
