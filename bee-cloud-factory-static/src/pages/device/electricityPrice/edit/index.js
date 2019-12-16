import { Component } from 'react'
import { Button, Card, Row, Col, Form, Input, DatePicker, Select, TimePicker, message } from 'antd'
import styles from './index.less'
import withRouter from 'umi/withRouter'
import BreadCrumb from '../../../../components/BreadCrumb'
import router from 'umi/router'
import {
  saveElectricityPrice,
  getElectricityPriceById,
  updateElectricityPrice
} from '../../services'
import moment from 'moment'

const Option = Select.Option
const FormItem = Form.Item
const { MonthPicker, RangePicker, WeekPicker } = DatePicker
const format = 'HH:mm'

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    startValue: null,
    endValue: null,
    startTime: null,
    endTime: null,
    startTimeDisable: []
  }

  id = this.props.location.query.id || null

  componentDidMount() {
    const { setFieldsValue } = this.props.form
    if (this.id) {
      getElectricityPriceById(this.id).then(res => {
        if (res.code === 1) {
          var data = res.object
          for (let key in data) {
            if (key === 'effectiveDate' || key === 'expirationDate') {
              data[key] = moment(data[key])
            }

            if (key === 'startTime' || key === 'endTime') {
              data[key] = data[key] ? moment(data[key], 'HH:mm') : moment("00:00", 'HH:mm')
            }
          }
          setFieldsValue(data)
        }
      })
    }
  }

  handleSubmit = () => {
    const { validateFields } = this.props.form

    validateFields((err, values) => {
      if (err) {
        return
      }
      for (let key in values) {
        if (key === 'effectiveDate' || key === 'expirationDate') {
          values[key] = moment(values[key]).format('YYYY-MM-DD')
        }
        if (key === 'startTime' || key === 'endTime') {
          values[key] = moment(values[key]).format('HH:mm')
        }
      }
      if (this.id) {
        values.id = this.id
        updateElectricityPrice(values).then(res => {
          if (res.code === 1) {
            message.success(res.message)
            router.goBack()
          } else {
            message.error(res.message)
          }
        })
      } else {
        saveElectricityPrice(values).then(res => {
          if (res.code === 1) {
            message.success(res.message)
            router.goBack()
          } else {
            message.error(res.message)
          }
        })
      }
    })
  }

  onStartChange = value => {
    this.setState({
      startValue: value
    })
  }

  onEndChange = value => {
    this.setState({
      endValue: value
    })
  }

  disabledEndDate = endValue => {
    const { startValue } = this.state
    if (!endValue || !startValue) {
      return false
    }

    return endValue.dayOfYear() < startValue.dayOfYear() + 1
  }

  disabledStartDate = startValue => {
    const { endValue } = this.state
    if (!startValue || !endValue) {
      return false
    }
    return startValue.dayOfYear() + 1 > endValue.dayOfYear()
  }

  onStartTimeChange = (time, timeString) => {
    this.setState({
      startTime: time
    })
    console.log(time, timeString)
  }

  onStartTimeChange = (time, timeString) => {
    this.setState({
      startTime: time
    })
    console.log(time, timeString)
  }

  startTimeDisable = (res, res_, des) => {
    if (this.state.startTime) {
      // console.log(this.state.startTime.duration().hours())
    } else {
      return []
    }
  }

  onStartTimeOpen = open => {
    if (open) {
      // this.startTimeDisable()
    }
  }

  render() {
    const { getFieldDecorator } = this.props.form
    const { startTimeDisable } = this.state
    return (
      <div>
        <BreadCrumb
          extra={
            <Button type="primary" onClick={() => router.goBack()}>
              返回
            </Button>
          }
        />
        <div className={styles.container}>
          <Card title="新增电价项" bordered={false}>
            <Form layout="inline">
              <Row>
                <Col span={24}>
                  <FormItem label="电价类型">
                    {getFieldDecorator('electricityType', {
                      rules: [
                        {
                          required: true,
                          message: '请选择电表类型'
                        }
                      ]
                    })(
                      <Select style={{ width: 300 }} placeholder="请选择">
                        <Option value={0}>炉变电</Option>
                        <Option value={1}>动力电</Option>
                      </Select>
                    )}
                  </FormItem>
                </Col>
                {/* <Col span={24}>
                  <FormItem label="日期区间">
                    {getFieldDecorator('name')(
                      <RangePicker style={{ width: 300 }} />
                    )}
                  </FormItem>
                </Col> */}
                <Col span={24}>
                  <FormItem label="生效日期">
                    {getFieldDecorator('effectiveDate', {
                      rules: [
                        {
                          required: true,
                          message: '请选择生效日期'
                        }
                      ]
                    })(
                      <DatePicker
                        style={{ width: 300 }}
                        disabledDate={this.disabledStartDate}
                        onChange={this.onStartChange}
                      />
                    )}
                  </FormItem>
                </Col>
                <Col span={24}>
                  <FormItem label="失效日期">
                    {getFieldDecorator('expirationDate')(
                      <DatePicker
                        style={{ width: 300 }}
                        disabledDate={this.disabledEndDate}
                        onChange={this.onEndChange}
                        onOpenChange={this.handleEndOpenChange}
                      />
                    )}
                  </FormItem>
                </Col>
                <Col span={24}>
                  <FormItem label="时间区间">
                    {getFieldDecorator('startTime', {
                      rules: [
                        {
                          required: true,
                          message: '请选择时间区间'
                        }
                      ]
                    })(
                      <TimePicker
                        style={{ width: 300 }}
                        onChange={this.onStartTimeChange}
                        // disabledSeconds={this.startTimeDisable}
                        // onOpenChange={this.onStartTimeOpen}
                        format={format}
                      />
                    )}
                  </FormItem>
                  <FormItem>
                    {getFieldDecorator('endTime', {
                      rules: [
                        {
                          required: true,
                          message: '请选择时间区间'
                        }
                      ]
                    })(
                      <TimePicker
                        style={{ width: 300 }}
                        onChange={this.onEndTimeChange}
                        format={format}
                      />
                    )}
                  </FormItem>
                </Col>
                <Col span={24}>
                  <FormItem label="电价(元/度)">
                    {getFieldDecorator('price', {
                      rules: [
                        {
                          required: true,
                          message: '请输入电价'
                        }
                      ]
                    })(<Input style={{ width: 300 }} placeholder="请输入" />)}
                  </FormItem>
                </Col>
              </Row>
            </Form>
          </Card>

          <div className={styles.btnBox}>
            <Button type="primary" onClick={this.handleSubmit}>
              保存
            </Button>
            <Button onClick={() => router.goBack()}>取消</Button>
          </div>
        </div>
      </div>
    )
  }
}
