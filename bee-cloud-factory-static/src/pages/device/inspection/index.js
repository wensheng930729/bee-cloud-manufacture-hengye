import { Component } from 'react'
import { Button, Card, Table, Divider, Form, Input, Select, Modal, Row, Col, message, Popconfirm } from 'antd'
import styles from './index.less'
import withRouter from 'umi/withRouter'
import BreadCrumb from '../../../components/BreadCrumb'
import router from 'umi/router'
import * as utils from '@/utils/utils'
import {
  searchDeviceInspectionList,
  deleteDeviceInspectionById,
  getDeviceListByName,
  saveDeviceInspection,
  getDeviceInspectionById,
  updateDeviceInspection
} from '../services'

const FormItem = Form.Item
const Option = Select.Option

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    data: [],
    currentPage: 1,
    pageSize: 10,
    totalPage: 0,
    totalRecords: 0,
    visible: false,
    selectList: []
  }

  id = null

  componentDidMount() {
    const { currentPage, pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  }

  getData({ currentPage, pageSize }) {
    const { validateFields } = this.props.form;
    validateFields(['deviceName'], (err, values) => {
      if (!err) {
        let obj = {
          currentPage,
          pageSize,
        };

        if (values.deviceName) {
          obj.deviceName = values.deviceName
        }
        const str = utils.queryString(obj)
        searchDeviceInspectionList(str).then(res => {
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

  handleReset = () => {
    const { resetFields } = this.props.form
    resetFields()
    this.getData({ currentPage: 1, pageSize: 10 });
  }

  onSearch = (val, callback) => {
    getDeviceListByName(val).then(res => {
      if (res.code === 1) {
        this.setState({
          selectList: res.object
        }, () => { callback && callback() })
      } else {
        message.error(res.message)
      }
    })
  }

  handleDelete = id => {
    deleteDeviceInspectionById(id).then(res => {
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
    const values = getFieldsValue(['code', 'inspectionItem', 'name'])
    const params = {
      ...values,
      deviceId: values.name.key,
      name: values.name.label
    }
    if (this.id) {
      params.id = this.id

      updateDeviceInspection(params).then(res => {
        this.id = null
        if (res.code === 1) {
          message.success(res.message)
          self.setState({ visible: false }, () => {
            resetFields(['name', 'code', 'inspectionItem'])
            this.getData({ currentPage: 1, pageSize: 10 });
          })
        } else {
          message.error(res.message)
        }
      })
    } else {
      saveDeviceInspection(params).then(res => {
        if (res.code === 1) {
          message.success(res.message)
          self.setState({ visible: false }, () => {
            resetFields(['name', 'code', 'inspectionItem'])
            this.getData({ currentPage: 1, pageSize: 10 });
          })
        } else {
          message.error(res.message)
        }
      })
    }
  }

  handleEdit = id => {
    this.id = id
    const { setFieldsValue } = this.props.form
    this.setState({
      visible: true
    }, () => {
      getDeviceInspectionById(id).then(res => {
        if (res.code === 1) {
          const data = res.object
          const obj = {
            key: data.deviceId,
            label: data.name
          }

          this.onSearch(data.name, () => {
            setFieldsValue({
              name: obj,
              code: data.code,
              inspectionItem: data.inspectionItem
            })
          })
        }
      })
    })
  }

  handleCancel = () => {
    const { resetFields } = this.props.form
    this.setState({
      visible: false
    }, () => {
      resetFields(['name', 'code', 'inspectionItem'])
    })
  }

  render() {
    const { getFieldDecorator } = this.props.form
    const { data, currentPage, pageSize, totalPage, totalRecords, visible, selectList } = this.state
    const columns = [
      {
        title: '设备名称',
        dataIndex: 'name',
        key: 'name'
      }, {
        title: '设备编号',
        dataIndex: 'code',
        key: 'code'
      }, {
        title: '巡检项目',
        dataIndex: 'inspectionItem',
        key: 'inspectionItem'
      }, {
        title: ' 创建时间',
        dataIndex: 'createTime',
        key: 'createTime'
      }, {
        title: ' 操作',
        key: 'actions',
        render: (text, row) => (
          <div>
            <span onClick={() => this.handleEdit(row.id)} style={{ color: '#1890ff', cursor: 'pointer' }}>编辑</span>
            <Divider type="vertical" />
            <Popconfirm placement="topRight" title={'确认删除该设备巡检？'} onConfirm={() => this.handleDelete(row.id)} okText="确认" cancelText="取消">
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
          <Card title="设备巡检" bordered={false}>
            <Form layout="inline">
              <Row type="flex" justify="space-between" style={{ width: '100%' }}>
                <Col>
                  <FormItem label="设备名称">
                    {getFieldDecorator('deviceName')(
                      <Input style={{ width: 260 }} autoComplete='off'/>
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
          title="新增设备巡检"
          visible={visible}
          onOk={this.handleOk}
          okText="保存"
          onCancel={this.handleCancel}
        >
          <Form layout="inline">
            <FormItem label="设备名称">
              {getFieldDecorator('name')(
                <Select
                  style={{ width: 300 }}
                  placeholder="请选择"
                  showSearch
                  optionFilterProp="children"
                  // onChange={onChange}
                  onSearch={this.onSearch}
                  filterOption={(input, option) =>
                    option.props.children
                      .toLowerCase()
                      .indexOf(input.toLowerCase()) >= 0
                  }
                  labelInValue
                >
                  {selectList.map((item, index) => {
                    return (
                      <Option value={item.id} key={item.id}>
                        {item.name}
                      </Option>
                    )
                  })}
                </Select>
              )}
            </FormItem>
            <FormItem style={{ marginTop: 24 }} label="巡检项目">
              {getFieldDecorator('inspectionItem')(
                <Input style={{ width: 300 }} placeholder="请输入/非必填" />
              )}
            </FormItem>
            <FormItem style={{ marginTop: 24 }} label="设备编号">
              {getFieldDecorator('code')(
                <Input style={{ width: 300 }} placeholder="请输入" />
              )}
            </FormItem>
          </Form>
        </Modal>
      </div>
    )
  }
}
