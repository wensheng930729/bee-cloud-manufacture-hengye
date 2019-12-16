import { Component } from 'react'
import { Button, Card, Table, Divider, Form, Input, Select, Row, Col, Modal, message, Popconfirm } from 'antd'
import styles from './index.less'
import withRouter from 'umi/withRouter'
import BreadCrumb from '../../../components/BreadCrumb'
import {
  searchWeighDeviceList,
  deleteWeighDeviceById,
  saveWeighDevice,
  updateWeighDevice,
  getWeighDeviceById
} from '../services'
import router from 'umi/router'
import * as utils from '@/utils/utils'

const FormItem = Form.Item
const Option = Select.Option
const typeList = ['地磅', '行车称']

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
    const { currentPage, pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  }

  getData({ currentPage, pageSize }) {
    const { validateFields } = this.props.form;
    validateFields(['weighDeviceName', 'status'], (err, values) => {
      if (!err) {
        let obj = {
          currentPage,
          pageSize,
          ...values
        };
        const str = utils.queryString(obj)
        searchWeighDeviceList(str).then(res => {
          if (res && res.code === 1 && res.object) {
            this.setState({
              data: res.object,
              ...res.page
            })
          }
        })
      }
    });
  }

  onChange = (currentPage) => {
    const { pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  }

  onShowSizeChange = (currentPage, pageSize) => {
    this.getData({ currentPage, pageSize });
  }

  handleSearch = () => {
    const { currentPage, pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  }

  handleCancel = () => {
    const { resetFields } = this.props.form
    this.setState({
      visible: false
    }, () => {
      resetFields(['status1', 'type', 'name'])
    })
  }

  handleEdit = id => {
    this.id = id
    const { setFieldsValue } = this.props.form
    this.setState({
      visible: true
    }, () => {
      getWeighDeviceById(id).then(res => {
        if (res.code === 1) {
          const data = res.object
          setFieldsValue({
            name: data.name,
            type: data.type,
            status1: data.status,
            deviceId: data.deviceId
          })
        }
      })
    })
  }

  handleDelete = id => {
    deleteWeighDeviceById(id).then(res => {
      if (res.code === 1) {
        message.success(res.message);
        this.getData({ currentPage: 1, pageSize: 10 });
      } else {
        message.error(res.message)
      }
    })
  }

  handleOk = () => {
    const { getFieldsValue, resetFields } = this.props.form
    const self = this
    const values = getFieldsValue(['status1', 'deviceId', 'name', 'type'])
    const params = {
      ...values,
      status: values.status1
    }

    if (this.id) {
      params.id = this.id

      updateWeighDevice(params).then(res => {
        this.id = null
        if (res.code === 1) {
          message.success(res.message)
          self.setState({ visible: false }, () => {
            resetFields(['status1', 'type', 'name', 'deviceId'])
            this.getData({ currentPage: 1, pageSize: 10 });
          })
        } else {
          message.error(res.message)
        }
      })
    } else {
      saveWeighDevice(params).then(res => {
        if (res.code === 1) {
          message.success(res.message)
          self.setState({ visible: false }, () => {
            resetFields(['status1', 'type', 'name', 'deviceId'])
            this.getData({ currentPage: 1, pageSize: 10 });
          })
        } else {
          message.error(res.message)
        }
      })
    }
  }

  handleReset = () => {
    const { resetFields } = this.props.form
    resetFields(['status', 'weighDeviceName'])
    this.getData({ currentPage: 1, pageSize: 10 });
  }

  render() {
    const { getFieldDecorator } = this.props.form
    const { data, currentPage, pageSize, totalPage, totalRecords, visible } = this.state
    const columns = [
      {
        title: '设备名称',
        dataIndex: 'name',
        key: 'name'
      }, {
        title: '设备id',
        dataIndex: 'deviceId',
        key: 'deviceId'
      }, {
        title: '称重设备类型',
        dataIndex: 'type',
        key: 'type',
        render(h) {
          return typeList[h]
        }
      }, {
        title: '状态',
        dataIndex: 'status',
        key: 'status',
        render: (text, row) => text ? <span style={{ color: '#1890FF' }}>启用</span> : <span style={{ color: '#f5222d' }}>未启用</span>
      }, {
        title: ' 操作',
        key: 'actions',
        render: (text, row) => (
          <div>
            <span onClick={() => this.handleEdit(row.id)} style={{ color: '#1890ff', cursor: 'pointer' }}>编辑</span>
            <Divider type="vertical" />
            <Popconfirm placement="topRight" title={'确认删除该称重设备？'} onConfirm={() => this.handleDelete(row.id)} okText="确认" cancelText="取消">
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
          <Card title="称重设备管理" bordered={false}>
            <Form layout="inline">
              <Row type="flex" justify="space-between" align="middle" style={{ width: '100%' }}>
                <Col>
                  <FormItem label="设备名称">
                    {getFieldDecorator('weighDeviceName')(
                      <Input style={{ width: 260 }} placeholder="设备名称" autoComplete='off'/>
                    )}
                  </FormItem>
                  <FormItem label="状态">
                    {getFieldDecorator('status', {})(
                      <Select style={{ width: 260 }} placeholder="请选择">
                        <Option value={0}>未启用</Option>
                        <Option value={1}>启用</Option>
                      </Select>
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
              pagination={{
                showQuickJumper: true,
                showSizeChanger: true,
                defaultCurrent: 1,
                defaultPageSize: 10,
                current: currentPage,
                pageSize: pageSize,
                total: totalRecords,
                onChange: this.onChange.bind(this),
                pageSizeOptions: ['10', '20', '30'],
                showTotal: (total, range) =>
                  `共 ${totalRecords} 条记录 第 ${currentPage} / ${totalPage} 页`,
                onShowSizeChange: this.onShowSizeChange.bind(this)
              }}
            />
          </Card>
        </div>

        <Modal
          title="新增设备"
          visible={visible}
          onOk={this.handleOk}
          okText="保存"
          onCancel={this.handleCancel}
        >
          <Form layout="inline">
            <FormItem label="设备名称">
              {getFieldDecorator('name')(
                <Input style={{ width: 300 }} placeholder="请输入" />
              )}
            </FormItem>
            <FormItem label="设备编号" style={{ marginTop: 24 }}>
              {getFieldDecorator('deviceId')(
                <Input style={{ width: 300 }} placeholder="请输入" />
              )}
            </FormItem>
            <FormItem style={{ marginTop: 24 }} label="设备类型">
              {getFieldDecorator('type')(
                <Select style={{ width: 300 }} placeholder="请选择">
                  {typeList.map((item, index) => {
                    return <Option value={index}>{item}</Option>
                  })}
                </Select>
              )}
            </FormItem>
            <FormItem style={{ marginTop: 24 }} label="启用状态">
              {getFieldDecorator('status1', {
                initialValue: 0
              })(
                <Select style={{ width: 300 }} placeholder="请选择">
                  <Option value={0}>未启用</Option>
                  <Option value={1}>启用</Option>
                </Select>
              )}
            </FormItem>
          </Form>
        </Modal>
      </div>
    )
  }
}
