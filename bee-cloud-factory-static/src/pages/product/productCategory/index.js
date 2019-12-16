import { Component, Fragment } from 'react'
import { Button, Card, Table, Divider, Form, Input, Badge, Row, Col, message, Select, Modal, Popconfirm } from 'antd'
import styles from './index.less'
import withRouter from 'umi/withRouter'
import BreadCrumb from '../../../components/BreadCrumb'
import router from 'umi/router'
import {
  getProductCategoryList,
  searchProductCategoryList,
  saveProductCategory,
  updateProductCategory,
  deleteProductCategoryById
} from '../services'
import * as utils from '@/utils/utils'

const FormItem = Form.Item
const Option = Select.Option
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
    getProductCategoryList: [],
    visible: false
  }

  id = null

  componentDidMount() {
    const { currentPage, pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  }

  getData(page) {
    const { validateFields } = this.props.form;
    validateFields(['status', 'name'], (err, values) => {
      if (!err) {
        const str = utils.queryString(page)
        searchProductCategoryList(str, values).then(res => {
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

  handleOk = () => {
    const { getFieldsValue, resetFields } = this.props.form
    const self = this
    const values = getFieldsValue(['status1', 'name1'])
    const params = {
      status: values.status1,
      name: values.name1
    }
    if (this.id) {
      params.id = this.id
      updateProductCategory(params).then(res => {
        if (res.code === 1) {
          this.id = null

          message.success(res.message)
          self.setState({ visible: false }, () => {
            resetFields(['status1', 'name1'])
            self.getData({ currentPage: 1, pageSize: 10 });
          })
        } else {
          message.error(res.message)
        }
      })
    } else {
      saveProductCategory(params).then(res => {
        if (res.code === 1) {
          message.success(res.message)
          self.setState({ visible: false }, () => {
            resetFields(['status1', 'name1'])
            self.getData({ currentPage: 1, pageSize: 10 });
          })
        } else {
          message.error(res.message)
        }
      })
    }
  }

  handleDelete = id => {
    deleteProductCategoryById(id).then(res => {
      if (res.code === 1) {
        message.success(res.message)
        this.getData({ currentPage: 1, pageSize: 10 });
      } else {
        message.error(res.message)
      }
    })
  }

  handleReset = () => {
    const { resetFields } = this.props.form
    resetFields(['status', 'name'])
    this.getData({ currentPage: 1, pageSize: 10 });
  }

  handleCancel = () => {
    const { resetFields } = this.props.form
    this.setState({
      visible: false
    }, () => {
      resetFields(['status1', 'name1'])
    })
  }

  handleEdit = row => {
    this.id = row.id
    const { setFieldsValue } = this.props.form
    this.setState({
      visible: true
    }, () => {
      setFieldsValue({
        name1: row.name,
        status1: row.status
      })
    })
  }

  render() {
    const { getFieldDecorator } = this.props.form

    const { data, currentPage, pageSize, totalPage, totalRecords, getProductCategoryList, visible } = this.state
    const columns = [
      {
        title: '产品类别名称',
        dataIndex: 'name',
        key: 'name'
      }, {
        title: '状态',
        dataIndex: 'status',
        key: 'status',
        render: (text, row) => text ? <span style={{ color: '#1890FF' }}>启用</span> : <span style={{ color: '#f5222d' }}>禁用</span>
      }, {
        title: ' 操作',
        key: 'actions',
        render: (text, row, index) => (
          <div>
            {row.name !== '主料' && row.name !== '辅料' && row.name !== '成品' ? (
              <Fragment>
                <span style={{ color: '#1890ff', cursor: 'pointer' }} onClick={() => this.handleEdit(row)}>编辑</span>
                <Divider type="vertical" />
                <Popconfirm
                  placement="topRight"
                  title={'确认删除该产品类别？'}
                  onConfirm={() => this.handleDelete(row.id)}
                  okText="确认"
                  cancelText="取消"
                >
                  <span style={{ color: '#1890ff', cursor: 'pointer' }}>删除</span>
                </Popconfirm>
              </Fragment>
            ) : null}
          </div>
        )
      }
    ]
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => this.setState({ visible: true })}>新增</Button>} />
        <div className={styles.container}>
          <Card title="产品类别" bordered={false}>
            <Form layout="inline">
              <Row type="flex" justify="space-between" style={{ width: '100%' }}>
                <Col>
                  <FormItem label="产品类别名称">
                    {getFieldDecorator('name')(
                      <Input
                        style={{ width: 260 }}
                        placeholder="按产品类别名称搜索"
                      />
                    )}
                  </FormItem>
                  <FormItem label="状态">
                    {getFieldDecorator('status')(
                      <Select style={{ width: 260 }} placeholder="请选择">
                        <Option value={1}>启用</Option>
                        <Option value={0}>禁用</Option>
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
          title="新增产品类别"
          visible={visible}
          onOk={this.handleOk}
          okText="保存"
          onCancel={this.handleCancel}
        >
          <Form {...formItemLayout}>
            <FormItem label="产品类别名称">
              {getFieldDecorator('name1')(
                <Input style={{ width: 300 }} placeholder="请输入" />
              )}
            </FormItem>
            <FormItem style={{ marginTop: 24 }} label="启用状态">
              {getFieldDecorator('status1', {
                initialValue: 0
              })(
                <Select style={{ width: 300 }} placeholder="请选择">
                  <Option value={0}>禁用</Option>
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
