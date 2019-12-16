import { Component } from 'react'
import { Button, Card, Row, Col, Form, Input, Select, message, Table } from 'antd'
import styles from './index.less'
import withRouter from 'umi/withRouter'
import BreadCrumb from '../../../../components/BreadCrumb'
import {
  savePlcDevice,
  getPlcDeviceById,
  updatePlcDevice,
  getFieldTypes
} from '../../services'
import router from 'umi/router'

const FormItem = Form.Item
const Option = Select.Option

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    data: [{
      fieldName: '',
      field: '',
      fieldType: null
    }],
    fields: []
  }

  id = this.props.location.query.id || null

  componentDidMount() {
    const { setFieldsValue } = this.props.form
    if (this.id) {
      getPlcDeviceById(this.id).then(res => {
        if (res.code === 1) {
          const data = res.object
          this.setState({
            data: data.fieldConfigs
          })
          setFieldsValue({
            brand: data.brand,
            status: data.status,
            type: data.type,
            name: data.name
          })
        }
      })
    }
    getFieldTypes().then(res => {
      if (res.code === 1) {
        this.setState({
          fields: res.object
        })
      } else {
        message.error(res.message)
      }
    })
  }

  handleSubmit = () => {
    const { getFieldsValue } = this.props.form
    const { data } = this.state
    const values = getFieldsValue()
    values.fieldConfig = data
    if (this.id) {
      values.id = this.id
      updatePlcDevice(values).then(res => {
        if (res.code === 1) {
          message.success(res.message)
          router.goBack()
        } else {
          message.error(res.message)
        }
      })
    } else {
      savePlcDevice(values).then(res => {
        if (res.code === 1) {
          message.success(res.message)
          router.goBack()
        } else {
          message.error(res.message)
        }
      })
    }
  }

  handleAdd = () => {
    let { data } = this.state
    const obj = {
      fieldName: '',
      field: '',
      fieldType: null
    }
    data.push(obj)
    this.setState({
      data
    })
  }

  handleDelete = index => {
    const { data } = this.state
    data.splice(index, 1)
    this.setState({
      data
    })
  }

  handleChange = (value, name, dataIndex) => {
    const { data } = this.state
    data[dataIndex][name] = value;
    this.setState({
      data
    })
  }

  render() {
    const { data, fields } = this.state
    const { getFieldDecorator } = this.props.form
    const self = this
    const columns = [
      {
        title: '下料斗名称',
        dataIndex: 'fieldName',
        width: '25%',
        render: (h, row, index) => <Input placeholder="下料斗名称" value={h} onChange={e => self.handleChange(e.target.value, 'fieldName', index)} />
      }, {
        title: 'id',
        dataIndex: 'field',
        width: '25%',
        render: (h, row, index) => <Input placeholder="下料斗id" value={h} style={{ width: '30%' }} onChange={e => self.handleChange(e.target.value, 'field', index)} />
      }, {
        title: '下料斗类型',
        dataIndex: 'fieldType',
        width: '25%',
        render: (h, row, index) => (
          <Select style={{ width: '50%' }} placeholder="下料斗类型" value={h} onChange={e => self.handleChange(e, 'fieldType', index)}>
            {
              fields.map((item, index) => <Option value={item.key} key={item.key}>{item.value}</Option>)
            }
          </Select>
        )
      }, {
        title: '操作',
        dataIndex: 'action',
        width: '25%',
        render: (h, row, index) => <span style={{ color: '#1890ff', cursor: 'pointer' }} onClick={() => self.handleDelete(index)}>删除</span>
      }
    ]
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.goBack()}>返回</Button>} />
        <div className={styles.container}>
          <Card title="基本信息" bordered={false} extra={
            <div className={styles.btnBox}>
              <Button type="primary" onClick={this.handleSubmit}>
                {this.id ? '修改' : '保存'}
              </Button>
              <Button onClick={() => router.goBack()}>取消</Button>
            </div>
          }>
            <Form>
              <Row style={{ marginLeft: '20%' }}>
                <Col span={12}>
                  <FormItem label="设备名称">
                    {getFieldDecorator('name', {
                      rules: [{ required: true, message: '请填入设备名称' }]
                    })(<Input style={{ width: 300 }} placeholder="请输入" />)}
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="品牌">
                    {getFieldDecorator('brand', {
                      rules: [{ required: true, message: '请填入品牌' }]
                    })(<Input style={{ width: 300 }} placeholder="请输入" />)}
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="型号">
                    {getFieldDecorator('type', {
                      rules: [{ required: true, message: '请填入型号' }]
                    })(<Input style={{ width: 300 }} placeholder="请输入" />)}
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="状态">
                    {getFieldDecorator('status', {
                      rules: [{ required: true, message: '请选择设备状态' }]
                    })(
                      <Select style={{ width: 300 }} placeholder="请选择">
                        <Option value={1}>启用</Option>
                        <Option value={0}>停用</Option>
                      </Select>
                    )}
                  </FormItem>
                </Col>
              </Row>
            </Form>
          </Card>
          <Card title="下料斗配置" bordered={false}>
            <Table
              rowKey="id"
              columns={columns}
              dataSource={data}
              pagination={false}
            />
            <div className={styles.btn_add} onClick={this.handleAdd}>添加一行</div>
          </Card>
        </div>
      </div>
    )
  }
}
