import { Component } from 'react'
import { Button, Card, Row, Col, Form, Input, Select, message, InputNumber } from 'antd'
import styles from './index.less'
import withRouter from 'umi/withRouter'
import BreadCrumb from '../../../../components/BreadCrumb'
import { saveAmmeter, getAmmeterById, updateAmmeter } from '../../services'
import router from 'umi/router'

const FormItem = Form.Item
const Option = Select.Option

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    data: {}
  }

  id = this.props.location.query.id || null

  componentDidMount() {
    const { setFieldsValue } = this.props.form
    if (this.id) {
      getAmmeterById(this.id).then(res => {
        if (res.code === 1) {
          setFieldsValue(res.object)
        }
      })
    }
  }

  handleSubmit = () => {
    const { getFieldsValue } = this.props.form
    const values = getFieldsValue()
    if (this.id) {
      values.id = this.id
      updateAmmeter(values).then(res => {
        if (res.code === 1) {
          message.success(res.message)
          router.goBack()
        } else {
          message.error(res.message)
        }
      })
    } else {
      saveAmmeter(values).then(res => {
        if (res.code === 1) {
          message.success(res.message)
          router.goBack()
        } else {
          message.error(res.message)
        }
      })
    }
  }

  render() {
    const { getFieldDecorator } = this.props.form
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.goBack()}>返回</Button>} />
        <div className={styles.container}>
          <Card title="新增电表" bordered={false}>
            <Form>
              <Row>
                <Col span={12}>
                  <FormItem label="电表名称">
                    {getFieldDecorator('name')(
                      <Input style={{ width: 300 }} placeholder="请输入" />
                    )}
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="电表编号">
                    {getFieldDecorator('code')(
                      <Input style={{ width: 300 }} placeholder="请输入" />
                    )}
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="电压倍率">
                    {getFieldDecorator('voltageRate')(
                      <InputNumber style={{ width: 300 }} placeholder="请输入" />
                    )}
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="电流倍率">
                    {getFieldDecorator('currentRate')(
                      <InputNumber style={{ width: 300 }} placeholder="请输入" />
                    )}
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="用电类型">
                    {getFieldDecorator('electricityType')(
                      <Select style={{ width: 300 }} placeholder="请选择">
                        <Option value={0}>炉变电</Option>
                        <Option value={1}>动力电</Option>
                      </Select>
                    )}
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="电表类型">
                    {getFieldDecorator('meterType')(
                      <Select style={{ width: 300 }} placeholder="请选择">
                        <Option value={0}>三相三线</Option>
                        <Option value={1}>三相四线</Option>
                      </Select>
                    )}
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="协议类型">
                    {getFieldDecorator('protocolType')(
                      <Select style={{ width: 300 }} placeholder="请选择">
                        <Option value={0}>130协议</Option>
                        <Option value={1}>1376.1协议</Option>
                      </Select>
                    )}
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="SIM卡号">
                    {getFieldDecorator('simNumber')(
                      <Input style={{ width: 300 }} placeholder="请输入" />
                    )}
                  </FormItem>
                </Col>
              </Row>
            </Form>
          </Card>

          <div className={styles.btnBox}>
            <Button type="primary" onClick={this.handleSubmit}>
              {this.id ? '修改' : '保存'}
            </Button>
            <Button onClick={() => router.goBack()}>取消</Button>
          </div>
        </div>
      </div>
    )
  }
}
