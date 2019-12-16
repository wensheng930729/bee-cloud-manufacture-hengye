import { Component } from 'react'
import { Button, Card, Table, Divider, Form, Input, Select, Modal, Row, Col, message } from 'antd'
import styles from './index.less'
import withRouter from 'umi/withRouter'
import BreadCrumb from '../../../components/BreadCrumb'
import router from 'umi/router';
import {
  searchMaterialsConsumptionList,
  updateMaterialsConsumption,
  getProductListByCategory,
  saveMaterialsConsumption,
  deleteMaterialsConsumptionById,
  getMaterialsConsumptionById
} from './services'
import * as utils from '@/utils/utils'

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
    productList: []
  }

  id = null

  componentDidMount() {
    this.getUserList()

    getProductListByCategory(5).then(res => {
      if (res.code === 1) {
        this.setState({
          productList: res.object
        })
      }
    })
  }

  getUserList(
    currentPage = this.state.currentPage,
    pageSize = 10,
    productName
  ) {
    let obj = { currentPage, pageSize }
    if (productName) {
      obj.productName = productName
    }
    const str = utils.queryString(obj)
    searchMaterialsConsumptionList(str).then(res => {
      if (res && res.code === 1 && res.object) {
        this.setState({
          data: res.object,
          currentPage: currentPage,
          pageSize: pageSize,
          totalPage: res.page.totalPage,
          totalRecords: res.page.totalRecords
        })
      }
    })
  }

  onChange = current => {
    const { currentPage, pageSize } = this.state
    this.handleSearch(1, current, pageSize)
    // this.getUserList()
  }

  onShowSizeChange = (current, size) => {
    this.setState(
      {
        pageSize: size
      },
      () => {
        const { currentPage, pageSize } = this.state
        this.handleSearch(1, currentPage, pageSize)
      }
    )
  }

  handleReset = () => {
    const { resetFields } = this.props.form

    resetFields(['productName'])
    this.getUserList()
  }

  handleOk = () => {
    const { getFieldsValue, resetFields } = this.props.form
    const { currentPage, pageSize } = this.state
    const self = this
    const values = getFieldsValue(['name'])
    const params = {
      productId: values.name.key,
      productName: values.name.label
    }
    if (this.id) {
      params.id = this.id

      updateMaterialsConsumption(params).then(res => {
        this.id = null
        if (res.code === 1) {
          message.success(res.message)
          self.setState({ visible: false }, () => {
            resetFields(['name'])
            this.handleSearch(1, currentPage, pageSize)
          })
        } else {
          message.error(res.message)
        }
      })
    } else {
      saveMaterialsConsumption(params).then(res => {
        if (res.code === 1) {
          message.success(res.message)
          self.setState({ visible: false }, () => {
            resetFields(['name'])
            this.handleSearch(1, currentPage, pageSize)
          })
        } else {
          message.error(res.message)
        }
      })
    }
  }

  handleCancel = () => {
    const { resetFields } = this.props.form

    this.setState(
      {
        visible: false
      },
      () => {
        resetFields(['name'])
      }
    )
  }
  handleEdit = id => {
    this.id = id
    const { setFieldsValue } = this.props.form
    this.setState(
      {
        visible: true
      },
      () => {
        getMaterialsConsumptionById(id).then(res => {
          if (res.code === 1) {
            const data = res.object
            const obj = {
              key: data.productId,
              label: data.productName
            }

            setFieldsValue({
              name: obj
            })
          }
        })
      }
    )
  }

  handleDelete = id => {
    const { currentPage, pageSize } = this.state

    deleteMaterialsConsumptionById(id).then(res => {
      this.handleSearch(1, currentPage, pageSize)
      message.success(res.message)
    })
  }

  handleSearch = (
    e,
    currentPage = this.state.currentPage,
    pageSize = this.state.pageSize
  ) => {
    const { getFieldValue } = this.props.form

    // const { currentPage, pageSize} = this.state
    const productName = getFieldValue('productName')
    this.getUserList(currentPage, pageSize, productName)
  }

  render() {
    const { getFieldDecorator } = this.props.form
    const {
      data,
      currentPage,
      pageSize,
      totalPage,
      totalRecords,
      visible,
      productList
    } = this.state
    const columns = [
      {
        title: '产品名称',
        dataIndex: 'productName',
        key: 'productName'
      },
      {
        title: '创建时间',
        dataIndex: 'createTime',
        key: 'createTime'
      },
      {
        title: ' 操作',
        key: 'actions',
        render: (text, row) => (
          <div>
            <span
              onClick={() => this.handleEdit(row.id)}
              style={{ color: '#1890ff', cursor: 'pointer' }}
            >
              编辑
            </span>
            <Divider type="vertical" />
            <span
              style={{ color: '#1890ff', cursor: 'pointer' }}
              onClick={() => this.handleDelete(row.id)}
            >
              删除
            </span>
          </div>
        )
      }
    ]
    return (
      <div>
        <BreadCrumb
          extra={
            <Button
              onClick={() => this.setState({ visible: true })}
              type="primary"
            >
              新增
            </Button>
          }
        />
        <div className={styles.container}>
          <Card title="原料吨耗配置" bordered={false}>
            <Form layout="inline">
              <Row
                type="flex"
                justify="space-between"
                style={{ width: '100%' }}
              >
                <Col>
                  <FormItem label="产品名称">
                    {getFieldDecorator('productName')(
                      <Input style={{ width: 260 }} />
                    )}
                  </FormItem>
                </Col>
                <Col>
                  <Button type="primary" onClick={this.handleSearch}>
                    查询
                  </Button>
                  <Button style={{ marginLeft: 24 }} onClick={this.handleReset}>
                    重置
                  </Button>
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
          title="新增原料吨耗配置"
          visible={visible}
          onOk={this.handleOk}
          okText="保存"
          onCancel={this.handleCancel}
        >
          <Form layout="inline">
            <FormItem label="产品名称">
              {getFieldDecorator('name')(
                <Select
                  style={{ width: 300 }}
                  placeholder="请选择"
                  labelInValue
                >
                  {productList.map(item => {
                    return <Option value={item.id}>{item.name}</Option>
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
