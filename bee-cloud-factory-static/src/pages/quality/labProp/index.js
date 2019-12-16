import { Component } from 'react'
import { Button, Card, Table, Divider, Form, Input, Select, Row, Col, Modal, message, DatePicker, Popconfirm } from 'antd'
import styles from './index.less'
import withRouter from 'umi/withRouter'
import BreadCrumb from '../../../components/BreadCrumb'
import {
  searchTestAttributeList,
  deleteTestAttributeById,
  saveTestAttribute,
  updateTestAttribute,
  getDeviceById
} from '../services'
import router from 'umi/router'
import * as utils from '@/utils/utils'
import moment from 'moment'

const { MonthPicker, RangePicker, WeekPicker } = DatePicker
const FormItem = Form.Item
const Option = Select.Option
const typeList = ['输入项', '输出项']
const formItemLayout = {
  labelCol: { span: 6 },
  wrapperCol: { span: 18 }
}

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    data: [],
    currentPage: 1,
    pageSize: 10,
    totalPage: 0,
    totalRecords: 0,
    visible: false
  }

  id = null

  componentDidMount() {
    this.getData()
  }

  getData(params) {
    const obj = { ...params }
    const str = utils.queryString(obj)
    searchTestAttributeList(str).then(res => {
      if (res && res.code === 1 && res.object) {
        this.setState({
          data: res.object
        })
      }
    })
  }

  handleSearch = () => {
    const { getFieldsValue } = this.props.form
    const values = getFieldsValue(['time', 'attributeName'])
    const params = {
      attributeName: values.attributeName
    }
    if (values.time) {
      values.time.map((item, index) => {
        if (!index) {
          params.startTime = moment(item).format('YYYY-MM-DD')
        } else {
          params.endTime = moment(item).format('YYYY-MM-DD')
        }
      })
    }
    this.getData(params)
  }

  handleCancel = () => {
    const { resetFields } = this.props.form

    this.setState({
      visible: false
    }, () => {
      resetFields(['attributeName1', 'type'])
    })
  }

  handleEdit = row => {
    this.id = row.id
    const { setFieldsValue } = this.props.form
    this.setState({
      visible: true
    }, () => {
      setFieldsValue({
        attributeName1: row.attributeName,
        type: row.type
      })
    })
  }

  handleDelete = id => {
    const { currentPage, pageSize } = this.state

    deleteTestAttributeById(id).then(res => {
      if (res.code === 1) {
        message.success(res.message)
        this.handleSearch()
      } else {
        message.error(res.message)
      }
    })
  }

  handleOk = () => {
    const { getFieldsValue, resetFields } = this.props.form
    const { currentPage, pageSize } = this.state
    const self = this
    const values = getFieldsValue(['attributeName1', 'type'])
    const params = {
      ...values,
      attributeName: values.attributeName1
    }

    if (this.id) {
      params.id = this.id

      updateTestAttribute(params).then(res => {
        this.id = null
        if (res.code === 1) {
          message.success(res.message)
          self.setState({ visible: false }, () => {
            resetFields(['attributeName1', 'type'])
            this.handleSearch()
          })
        } else {
          message.error(res.message)
        }
      })
    } else {
      saveTestAttribute(params).then(res => {
        if (res.code === 1) {
          message.success(res.message)
          self.setState({ visible: false }, () => {
            resetFields(['attributeName1', 'type'])
            this.handleSearch()
          })
        } else {
          message.error(res.message)
        }
      })
    }
  }

  handleReset = () => {
    const { resetFields } = this.props.form

    resetFields(['time', 'attributeName'])
    this.getData({})
  }

  render() {
    const { getFieldDecorator } = this.props.form
    const { data, visible } = this.state
    const columns = [
      {
        title: '属性编码',
        dataIndex: 'id',
        key: 'id'
      }, {
        title: '化验属性名称',
        dataIndex: 'attributeName',
        key: 'attributeName'
      }, {
        title: '类型',
        dataIndex: 'type',
        key: 'type',
        render: (h) => typeList[h]
      }, {
        title: '创建日期',
        dataIndex: 'createTime',
        key: 'createTime'
      }, {
        title: ' 操作',
        key: 'actions',
        render: (text, row) => (
          <div>
            <span onClick={() => this.handleEdit(row)} style={{ color: '#1890ff', cursor: 'pointer' }}>编辑</span>
            <Divider type="vertical" />
            <Popconfirm
              placement="topRight"
              title={'确认删除该化验属性？'}
              onConfirm={() => this.handleDelete(row.id)}
              okText="确认"
              cancelText="取消"
            >
              <span style={{ color: '#1890ff', cursor: 'pointer' }}>删除</span>
            </Popconfirm>
          </div>
        )
      }
    ]
    return (
      <div>
        <BreadCrumb extra={<Button onClick={() => this.setState({ visible: true })} type="primary">新增</Button>} />
        <div className={styles.container}>
          <Card title="化验属性管理" bordered={false}>
            <Form layout="inline">
              <Row type="flex" justify="space-between" align="middle" style={{ width: '100%' }}>
                <Col>
                  <FormItem label="化验属性">
                    {getFieldDecorator('attributeName')(
                      <Input
                        style={{ width: 260 }}
                        placeholder="按属性名称搜索"
                      />
                    )}
                  </FormItem>
                  <FormItem label="创建日期">
                    {getFieldDecorator('time', {})(
                      <RangePicker style={{ width: 260 }} />
                    )}
                  </FormItem>
                </Col>
                <Col>
                  <Button type="primary" onClick={this.handleSearch}>查询</Button>
                  <Button style={{ marginLeft: 24 }} onClick={this.handleReset}>重置</Button>
                </Col>
              </Row>
            </Form>
            <Table
              rowKey="id"
              columns={columns}
              dataSource={data}
              pagination={false}
            // pagination={{
            //   showQuickJumper: true,
            //   showSizeChanger: true,
            //   defaultCurrent: 1,
            //   defaultPageSize: 10,
            //   current: currentPage,
            //   pageSize: pageSize,
            //   total: totalRecords,
            //   onChange: this.onChange.bind(this),
            //   pageSizeOptions: ['10', '20', '30'],
            //   showTotal: (total, range) =>
            //     `共 ${totalRecords} 条记录 第 ${currentPage} / ${totalPage} 页`,
            //   onShowSizeChange: this.onShowSizeChange.bind(this)
            // }}
            />
          </Card>
        </div>

        <Modal
          title="新增化验属性"
          visible={visible}
          onOk={this.handleOk}
          okText="保存"
          onCancel={this.handleCancel}
        >
          <Form {...formItemLayout}>
            <FormItem label="化验属性名称">
              {getFieldDecorator('attributeName1')(
                <Input style={{ width: 300 }} placeholder="请输入" />
              )}
            </FormItem>
            <FormItem style={{ marginTop: 24 }} label="化验类型">
              {getFieldDecorator('type')(
                <Select style={{ width: 300 }} placeholder="请选择">
                  {typeList.map((item, index) => {
                    return <Option value={index}>{item}</Option>
                  })}
                </Select>
              )}
            </FormItem>
          </Form>
        </Modal>
      </div>
    )
  }
}
