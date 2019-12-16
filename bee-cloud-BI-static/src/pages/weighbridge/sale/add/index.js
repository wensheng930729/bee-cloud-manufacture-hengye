import { Component } from 'react';
import { Form, Card, Input, Select, Button, Divider, InputNumber, message, } from 'antd';
import styles from './index.less';
import withRouter from 'umi/withRouter';
import router from 'umi/router';
import moment from 'moment';
import * as utils from '@/utils/utils';
import {
  getCarrierInfoList,
  getLastCarrierByTrainNumber,
  getAllCustomerAndSupplier,
  getWeightSelectList,
  getProductListByCategory,
  getWeight,
  saveSaleWeightMachine
} from '../services/index';

const FormItem = Form.Item;
const Option = Select.Option;
const { TextArea } = Input;

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    carriers: [], //承运方
    people: [], //收货单位
    product: [], //产品名称
    devices: [], //称重设备
    buttonState: 0, // 0-进厂称重；1-确认称重；2-手动修改；3-确认称重（手动）
    inFactoryWeightByManual: 0, // 0-自动，1-手动
    inFactoryWeight: null,
    arrivalTime: moment().format("YYYY-MM-DD HH:mm:ss")
  }

  componentDidMount() {
    getCarrierInfoList().then(res => {
      if (res && res.code === 1) {
        this.setState({
          carriers: res.object
        })
      }
    })
    getWeightSelectList().then(res => {
      if (res && res.code === 1) {
        this.setState({
          devices: res.object
        })
      }
    })
    getAllCustomerAndSupplier().then(res => {
      if (res && res.code === 1) {
        this.setState({
          people: res.object
        })
      }
    })
    getProductListByCategory().then(res => {
      if (res && res.code === 1) {
        this.setState({
          product: res.object
        })
      }
    })
  }

  getTrain = () => {
    const { validateFields, setFieldsValue } = this.props.form;
    validateFields(["trainNumber"], (err, values) => {
      if (!err && values.trainNumber) {
        let params = {
          trainNumber: values.trainNumber,
          type: 2
        }
        const paramsString = utils.queryString(params)
        getLastCarrierByTrainNumber(paramsString).then(res => {
          if (res && res.code === 1) {
            setFieldsValue({
              carrierId: res.object.carrierId || null
            })
          }
        })
      }
    })
  }

  getWeight = () => {
    const { validateFields } = this.props.form;
    validateFields(["deviceId"], (err, values) => {
      if (!err && values.deviceId) {
        getWeight(values.deviceId).then(res => {
          if (res && res.code === 1) {
            if (res.object === '0') {
              this.setState({
                inFactoryWeight: 0,
                buttonState: 2
              })
            } else {
              this.setState({
                inFactoryWeight: Number(res.object),
                buttonState: 1
              })
            }
          }
        })
      }
    })
  }

  handleSave = () => {
    const { validateFields } = this.props.form;
    const { carriers, people, product, inFactoryWeightByManual, inFactoryWeight, arrivalTime } = this.state;
    validateFields((err, values) => {
      if (!err) {
        let carriersItem = carriers.filter(item => {
          return values.carrierId === item.carrierId
        })
        let peopleItem = people.filter(item => {
          return values.receivingCompanyId === item.id
        })
        let productItem = product.filter(item => {
          return values.productId === item.id
        })
        let params = {
          ...values,
          carrierName: carriersItem.length ? carriersItem[0].carrierName : undefined,
          receivingCompany: peopleItem.length ? peopleItem[0].name : undefined,
          productName: productItem.length ? productItem[0].name : undefined,
          inFactoryWeightByManual,
          arrivalTime
        }
        saveSaleWeightMachine(params).then(res => {
          if (res && res.code === 1) {
            message.success(res.message);
            router.goBack();
          } else {
            message.error(res.message);
          }
        })
      }
    })
  }

  render() {
    const { getFieldDecorator } = this.props.form;
    const { carriers, people, product, devices, buttonState, inFactoryWeight, arrivalTime } = this.state;
    return (
      <div className={styles.container}>
        <Card title="过磅车辆" bordered={false}>
          <div className={styles.formPart}>
            <span className={styles.partTitle}>车辆信息</span>
            <Form>
              <FormItem label="车牌号">
                {
                  getFieldDecorator("trainNumber", {
                    rules: [{ required: true, message: '请输入车牌号' }]
                  })(
                    <Input style={{ width: '80%' }} placeholder="请输入车牌号" onBlur={this.getTrain.bind(this)} />
                  )
                }
              </FormItem>
              <FormItem label="司机">
                {
                  getFieldDecorator("driver")(
                    <Input style={{ width: '80%' }} placeholder="请输入司机" />
                  )
                }
              </FormItem>
              <FormItem label="联系方式">
                {
                  getFieldDecorator("contact")(
                    <Input style={{ width: '80%' }} placeholder="请输入联系方式" />
                  )
                }
              </FormItem>
              <FormItem label="承运方">
                {
                  getFieldDecorator("carrierId", {
                    rules: [{ required: true, message: '请选择承运方' }]
                  })(
                    <Select style={{ width: '80%' }} placeholder="请选择承运方">
                      {
                        carriers.map(item => <Option value={item.carrierId} key={item.carrierId}>{item.carrierName}</Option>)
                      }
                    </Select>
                  )
                }
              </FormItem>
              <FormItem label="收货单位">
                {
                  getFieldDecorator("receivingCompanyId", {
                    rules: [{ required: true, message: '请选择收货单位' }]
                  })(
                    <Select style={{ width: '80%' }} placeholder="请选择收货单位">
                      {
                        people.map(item => <Option value={item.id} key={item.id}>{item.name}</Option>)
                      }
                    </Select>
                  )
                }
              </FormItem>
              <FormItem label="产品名称">
                {
                  getFieldDecorator("productId", {
                    rules: [{ required: true, message: '请选择产品名称' }]
                  })(
                    <Select style={{ width: '80%' }} placeholder="请选择产品名称">
                      {
                        product.map(item => <Option value={item.id} key={item.id}>{item.name}</Option>)
                      }
                    </Select>
                  )
                }
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
                    <InputNumber disabled={buttonState !== 3} precision={2} style={{ width: '60%' }} placeholder="请称重" />
                  )
                }
                {
                  buttonState === 0 ? <Button onClick={this.getWeight.bind(this)} type="primary" style={{ marginLeft: 24 }}>进厂称重</Button> :
                    buttonState === 1 ? <Button onClick={() => this.setState({ buttonState: 2 })} type="primary" style={{ marginLeft: 24 }}>确认称重</Button> :
                      buttonState === 2 ? <Button onClick={() => this.setState({ buttonState: 3, inFactoryWeightByManual: 1 })} type="primary" style={{ marginLeft: 24 }}>手动修改</Button> :
                        buttonState === 3 ? <Button onClick={() => this.setState({ buttonState: 2 })} type="primary" style={{ marginLeft: 24 }}>确认称重</Button> : null
                }
              </FormItem>
              <FormItem label="实际到厂时间">
                {arrivalTime}
              </FormItem>
              <FormItem label="备注">
                {
                  getFieldDecorator("remark")(
                    <TextArea style={{ width: '80%' }} autoSize={{ minRows: 4, maxRows: 4 }} />
                  )
                }
              </FormItem>
            </Form>
          </div>

          <div className={styles.footer}>
            <Button onClick={() => router.goBack()}>返回</Button>
            <Button onClick={this.handleSave.bind(this)} type="primary">提交</Button>
          </div>
        </Card>
      </div>
    )
  }
}