import { Component } from 'react'
import { Button, Card, Table, Divider, Form, Input, Badge, Row, Col, message, Select } from 'antd'
import styles from './index.less'
import withRouter from 'umi/withRouter'
import BreadCrumb from '../../../components/BreadCrumb'
import router from 'umi/router'
import { getProductCategoryList, searchProductList } from '../services'
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
    getProductCategoryList: []
  }

  componentDidMount() {
    const { currentPage, pageSize } = this.state;
    this.getData({ currentPage, pageSize });
    getProductCategoryList().then(res => {
      if (res.code === 1) {
        this.setState({
          getProductCategoryList: res.object
        })
      }
    })
  }

  getData(page) {
    const { validateFields } = this.props.form;
    validateFields(['categoryId', 'status', 'name'], (err, values) => {
      if (!err) {
        const str = utils.queryString(page)
        searchProductList(str, values).then(res => {
          if (res && res.code === 1 && res.object) {
            this.setState({
              data: res.object,
              ...res.page
            })
          } else {
            message.error(res.message)
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

  handleDelete = id => {
    deleteAmmeterById(id).then(res => {
      if (res.code === 1) {
        message.success(res.message);
        this.getData({ currentPage: 1, pageSize: 10 });
      } else {
        message.error(res.message)
      }
    })
  }

  handleReset = () => {
    const { resetFields } = this.props.form
    resetFields(['categoryId', 'status', 'name'])
    this.getData({ currentPage: 1, pageSize: 10 });
  }

  render() {
    const { getFieldDecorator } = this.props.form
    const { data, currentPage, pageSize, totalPage, totalRecords, getProductCategoryList } = this.state
    const columns = [
      {
        title: '产品名称',
        dataIndex: 'name',
        key: 'name'
      }, {
        title: '产品类别',
        dataIndex: 'categoryName',
        key: 'categoryName'
      }, {
        title: '是否是标准品',
        dataIndex: 'standard',
        key: 'standard',
        render: (text, row) => text === 1 ? '是' : '否'
      }, {
        title: '计量单位',
        dataIndex: 'unitValue',
        key: 'unitValue'
      }, {
        title: '状态',
        dataIndex: 'status',
        key: 'status',
        render: (text, row) =>
          text ? <span style={{ color: '#1890FF' }}>启用</span> : <span style={{ color: '#f5222d' }}>禁用</span>
      }, {
        title: ' 操作',
        key: 'actions',
        render: (text, row) => (
          <div>
            <span onClick={() => router.push(`/product/archives/specConfig?id=${row.id}`)} style={{ color: '#1890ff', cursor: 'pointer' }}>规格配置</span>
            <Divider type="vertical" />
            <span onClick={() => router.push(`/product/archives/test?id=${row.id}`)} style={{ color: '#1890ff', cursor: 'pointer' }}>化验配置</span>
            <Divider type="vertical" />
            <span style={{ color: '#1890ff', cursor: 'pointer' }} onClick={() => router.push(`/product/archives/add?id=${row.id}`)}>编辑</span>
          </div>
        )
      }
    ]
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.push('/product/archives/add')}>新增</Button>} />
        <div className={styles.container}>
          <Card title="档案管理" bordered={false}>
            <Form layout="inline">
              <Row type="flex" justify="space-between" style={{ width: '100%' }}>
                <Col>
                  <FormItem label="产品名称">
                    {getFieldDecorator('name')(
                      <Input
                        style={{ width: 260 }}
                        placeholder="按产品名称搜索"
                      />
                    )}
                  </FormItem>
                  <FormItem label="产品类别">
                    {getFieldDecorator('categoryId')(
                      <Select style={{ width: 260 }} placeholder="请选择">
                        {getProductCategoryList.map((item, index) => {
                          return (
                            <Option value={item.id} key={item.id}>
                              {item.name}
                            </Option>
                          )
                        })}
                      </Select>
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
      </div>
    )
  }
}
