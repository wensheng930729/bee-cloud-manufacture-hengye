import { Component } from 'react';
import { Form, Card, Input, Select, Button, Divider, InputNumber, message, Popconfirm } from 'antd';
import styles from './index.less';
import withRouter from 'umi/withRouter';
import router from 'umi/router';
import * as utils from '@/utils/utils';
import {
  getWeightSelectList,
  getWeight,
  getWeightMachineWebDeatil,
  confirmWeight,
  confirmDeductWeight,
  saveRemark,
  printPoundSheet,
  continueMachine,
  confirmMachine
} from '../services/index';

const FormItem = Form.Item;
const Option = Select.Option;
const { TextArea } = Input;

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    detail: {},
    devices: [], //称重设备
    inHaveConfirm: false, // 是否已确认称重
    inButtonState: 0, // 0-进厂称重；1-确认称重；2-手动修改；3-确认称重（手动）
    inFactoryWeightByManual: 0, // 0-自动，1-手动
    inFactoryWeight: null,

    outHaveConfirm: false, // 是否已确认称重
    outButtonState: 0, // 0-出厂称重；1-确认称重；2-手动修改；3-确认称重（手动）
    outFactoryWeightByManual: 0, // 0-自动，1-手动
    outFactoryWeight: null,
    deductWeight: null,
    remark: null,
    deviceId: null
  }

  componentDidMount() {
    const { machineId } = this.props.location.query;
    getWeightSelectList().then(res => {
      if (res && res.code === 1) {
        this.setState({
          devices: res.object
        })
      }
    })

    const str = utils.queryString({ machineId, type: 2 });
    getWeightMachineWebDeatil(str).then(res => {
      if (res && res.code === 1) {
        this.setState({
          detail: res.object,

          inHaveConfirm: res.object.inFactoryWeightIsConfirm ? true : false,
          inButtonState: res.object.inFactoryWeightIsConfirm ? 2 : (res.object.inFactoryWeight ? 1 : 0),
          inFactoryWeightByManual: res.object.inFactoryWeightByManual || 0,
          inFactoryWeight: res.object.inFactoryWeight || res.object.inFactoryWeight === 0 ? res.object.inFactoryWeight : null,

          outHaveConfirm: res.object.outFactoryWeightIsConfirm ? true : false,
          outButtonState: res.object.outFactoryWeightIsConfirm ? 2 : (res.object.outFactoryWeight ? 1 : 0),
          outFactoryWeightByManual: res.object.outFactoryWeightByManual || 0,
          outFactoryWeight: res.object.outFactoryWeight || res.object.outFactoryWeight === 0 ? res.object.outFactoryWeight : null,

          deductWeight: res.object.deductWeight || null,
          remark: res.object.remark || null,
          deviceId: res.object.deviceId || null,
        })
      }
    })
  }

  getWeight = (type) => {
    const { validateFields } = this.props.form;
    validateFields(["deviceId"], (err, values) => {
      if (!err && values.deviceId) {
        getWeight(values.deviceId).then(res => {
          if (res && res.code === 1) {
            if (res.object === '0') {
              this.setState({
                [`${type}FactoryWeight`]: 0,
                [`${type}ButtonState`]: 2
              })
            } else {
              this.setState({
                [`${type}FactoryWeight`]: Number(res.object),
                [`${type}ButtonState`]: 1
              })
            }
          }
        })
      }
    })
  }

  handleWeight = (type) => {
    const { machineId } = this.props.location.query;
    const { detail, inFactoryWeight, inFactoryWeightByManual, outFactoryWeight, outFactoryWeightByManual } = this.state;
    let params = {
      machineId,
      type, //1进厂-2出厂
      weightType: 2, //1采购-2销售
    }

    if (type === 1) {
      params = {
        ...params,
        inFactoryWeight,
        inFactoryWeightByManual
      }
    } else {
      params = {
        ...params,
        outFactoryWeight,
        outFactoryWeightByManual
      }
    }

    confirmWeight(params).then(res => {
      if (res && res.code === 1) {
        message.success(res.message);
        if (type === 1) {
          this.setState({
            inButtonState: 2,
            inHaveConfirm: true,
            detail: {
              ...detail,
              inFactoryWeight
            }
          })
        } else {
          this.setState({
            outButtonState: 2,
            outHaveConfirm: true,
            detail: {
              ...detail,
              outFactoryWeight
            }
          })
        }
      }
    })
  }

  saveWeightAndRemark = (func) => {
    const { validateFields } = this.props.form;
    const { machineId } = this.props.location.query;

    let haveConfirmDeductWeight = true, haveConfirmRemark = true;

    validateFields(["deductWeight", "remark"], (err, values) => {
      if (!err) {
        if (values.deductWeight || values.deductWeight === 0) {
          haveConfirmDeductWeight = false;
          const str = utils.queryString({ machineId, type: 2, deductWeightByManual: 2, deductWeight: values.deductWeight });
          confirmDeductWeight(str).then(res => {
            if (res && res.code === 1) {
              haveConfirmDeductWeight = true;
              if (values.remark) {
                haveConfirmRemark = false;
                const str = utils.queryString({ machineId, type: 2, remark: values.remark });
                saveRemark(str).then(res => {
                  if (res && res.code === 1) {
                    haveConfirmRemark = true;
                    if (haveConfirmDeductWeight && haveConfirmRemark) {
                      func()
                    }
                  } else {
                    message.error(res.message)
                  }
                })
              } else {
                func()
              }
            } else {
              message.error(res.message)
            }
          })
        } else if (values.remark) {
          haveConfirmRemark = false;
          const str = utils.queryString({ machineId, type: 2, remark: values.remark });
          saveRemark(str).then(res => {
            if (res && res.code === 1) {
              haveConfirmRemark = true;
              if (haveConfirmDeductWeight && haveConfirmRemark) {
                func()
              }
            } else {
              message.error(res.message)
            }
          })
        } else {
          func()
        }
      }
    })
  }

  handleBack = () => {
    this.saveWeightAndRemark(() => {
      router.goBack()
    })
  }

  handlePrint = () => {
    this.saveWeightAndRemark(() => {
      const { validateFields } = this.props.form;
      const { machineId } = this.props.location.query;
      const {
        detail,
        deductWeight
      } = this.state;

      validateFields(["deviceId", "inFactoryWeight", "outFactoryWeight", "deductWeight", "remark"], (err, values) => {
        if (!err) {
          let params = {
            machineId,
            type: 2,
            remark: values.remark ? values.remark : '',
            deductWeight: values.deductWeight ? values.deductWeight : undefined,
            deviceId: values.deviceId,
            deliveryCompany: detail.receivingCompany,
            driver: detail.driver,
            productId: detail.productId,
            productName: detail.productName,
            trainNumber: detail.trainNumber,
            inFactoryWeight: detail.inFactoryWeight,
            outFactoryWeight: detail.outFactoryWeight,
            netWeight: (detail.outFactoryWeight - detail.inFactoryWeight - (values.deductWeight || 0)).toFixed(2),
          }
          printPoundSheet(params).then(res => {
            if (res && res.code === 1) {
              message.success(res.message);
            } else {
              message.error(res.message)
            }
          })
        }
      })
    })
  }

  handleOk = () => {
    const { validateFields } = this.props.form;
    const { machineId } = this.props.location.query;

    validateFields(["deductWeight", "remark"], (err, values) => {
      if (!err) {
        continueMachine({
          machineId,
          type: 2,
          deductWeight: values.deductWeight || values.deductWeight === 0 ? values.deductWeight : undefined,
          remark: values.remark ? values.remark : undefined
        }).then(res => {
          if (res && res.code === 1) {
            message.success(res.message);
            router.goBack();
          } else {
            message.error(res.message)
          }
        })
      }
    })
  }

  handleCancel = () => {
    const { validateFields } = this.props.form;
    const { machineId } = this.props.location.query;

    validateFields(["deductWeight", "remark"], (err, values) => {
      if (!err) {
        confirmMachine({
          machineId,
          type: 2,
          deductWeight: values.deductWeight || values.deductWeight === 0 ? values.deductWeight : undefined,
          remark: values.remark ? values.remark : undefined
        }).then(res => {
          if (res && res.code === 1) {
            message.success(res.message);
            router.goBack();
          } else {
            message.error(res.message)
          }
        })
      }
    })
  }

  render() {
    const { getFieldDecorator } = this.props.form;
    const { detail, devices, inHaveConfirm, inButtonState, inFactoryWeight, outHaveConfirm, outButtonState, outFactoryWeight, deductWeight, remark, deviceId } = this.state;
    return (
      <div className={styles.container}>
        <Card title="过磅车辆" bordered={false}>
          <div className={styles.formPart}>
            <span className={styles.partTitle}>车辆信息</span>
            <Form layout="inline">
              <FormItem label="车牌号">
                {detail.trainNumber || '未知'}
              </FormItem>
              <FormItem label="司机">
                {detail.driver || '未知'}
              </FormItem>
              <FormItem label="联系方式">
                {detail.contact || '未知'}
              </FormItem>
              <FormItem label="承运方">
                {detail.carrierName || '未知'}
              </FormItem>
              <FormItem label="收货单位">
                {detail.receivingCompany || '未知'}
              </FormItem>
              <FormItem label="产品名称">
                {detail.productName || '未知'}
              </FormItem>
            </Form>
          </div>
          <Divider />
          <div className={styles.formPart}>
            <span className={styles.partTitle}>称重信息</span>
            <Form>
              <FormItem label="称重设备">
                {
                  getFieldDecorator("deviceId", {
                    initialValue: deviceId,
                    rules: [{ required: true, message: '请选择称重设备' }]
                  })(
                    <Select style={{ width: '80%' }} placeholder="请选择称重设备">
                      {
                        devices.map((item, index) => <Option value={item.deviceId} key={item.deviceId}>{item.name}</Option>)
                      }
                    </Select>
                  )
                }
              </FormItem>
              <FormItem label="进厂数量">
                {
                  getFieldDecorator("inFactoryWeight", {
                    initialValue: inFactoryWeight,
                    rules: [{ required: true, message: '请称重' }]
                  })(
                    <InputNumber disabled={inButtonState !== 3} precision={2} style={{ width: '60%' }} placeholder="请称重" onChange={(value) => this.setState({ inFactoryWeight: value })} />
                  )
                }
                {
                  inButtonState === 0 ? <Button onClick={this.getWeight.bind(this, 'in')} type="primary" style={{ marginLeft: 24 }}>进厂称重</Button> :
                    inButtonState === 1 ? <Button onClick={this.handleWeight.bind(this, 1)} type="primary" style={{ marginLeft: 24 }}>确认称重</Button> :
                      inButtonState === 2 ? <Button onClick={() => this.setState({ inButtonState: 3, inFactoryWeightByManual: 1 })} type="primary" style={{ marginLeft: 24 }}>手动修改</Button> :
                        inButtonState === 3 ? <Button onClick={this.handleWeight.bind(this, 1)} type="primary" style={{ marginLeft: 24 }}>确认称重</Button> : null
                }
              </FormItem>
              <FormItem label="出厂数量">
                {
                  getFieldDecorator("outFactoryWeight", {
                    initialValue: outFactoryWeight,
                    rules: [{ required: true, message: '请称重' }]
                  })(
                    <InputNumber disabled={outButtonState !== 3} precision={2} style={{ width: '60%' }} placeholder="请称重" onChange={(value) => this.setState({ outFactoryWeight: value })} />
                  )
                }
                {
                  outButtonState === 0 ? <Button onClick={this.getWeight.bind(this, 'out')} type="primary" style={{ marginLeft: 24 }}>出厂称重</Button> :
                    outButtonState === 1 ? <Button onClick={this.handleWeight.bind(this, 2)} type="primary" style={{ marginLeft: 24 }}>确认称重</Button> :
                      outButtonState === 2 ? <Button onClick={() => this.setState({ outButtonState: 3, outFactoryWeightByManual: 1 })} type="primary" style={{ marginLeft: 24 }}>手动修改</Button> :
                        outButtonState === 3 ? <Button onClick={this.handleWeight.bind(this, 2)} type="primary" style={{ marginLeft: 24 }}>确认称重</Button> : null
                }
              </FormItem>
              <FormItem label="扣重数量">
                {
                  getFieldDecorator("deductWeight", {
                    initialValue: deductWeight || null,
                  })(
                    <InputNumber style={{ width: '80%' }} precision={2} placeholder="请输入扣重数量" onChange={(value) => this.setState({ deductWeight: value })} />
                  )
                }
              </FormItem>
              <FormItem label="净重">
                {inFactoryWeight && outFactoryWeight ? (outFactoryWeight - inFactoryWeight - (deductWeight || 0)).toFixed(2) : '无'}
              </FormItem>
              <FormItem label="实际到厂时间">
                {detail.arrivalTime || '未知'}
              </FormItem>
              <FormItem label="备注">
                {
                  getFieldDecorator("remark", {
                    initialValue: remark || null,
                  })(
                    <TextArea style={{ width: '80%' }} autoSize={{ minRows: 4, maxRows: 4 }} />
                  )
                }
              </FormItem>
            </Form>
          </div>

          <div className={styles.footer}>
            <Button onClick={this.handleBack.bind(this)} style={{ marginRight: 24 }}>返回</Button>
            <Button disabled={!inHaveConfirm || !outHaveConfirm} onClick={this.handlePrint.bind(this)} type="primary" style={{ marginRight: 24 }}>打印磅单</Button>
            <Popconfirm
              placement="topLeft"
              title="是否继续运载货物?"
              onConfirm={this.handleOk}
              onCancel={this.handleCancel}
              okText="继续运载"
              cancelText="结束运载"
              disabled={!inHaveConfirm || !outHaveConfirm}
            >
              <Button disabled={!inHaveConfirm || !outHaveConfirm} type="primary">提交</Button>
            </Popconfirm>
          </div>
        </Card>
      </div>
    )
  }
}