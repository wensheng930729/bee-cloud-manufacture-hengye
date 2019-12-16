import { Component } from 'react'
import { Button, Card, Table, Divider, Form, Input, Badge, Row, Col, message, Popconfirm } from 'antd'
import styles from './index.less'
import withRouter from 'umi/withRouter'
import BreadCrumb from '../../../components/BreadCrumb'
import router from 'umi/router'
import { searchAmmeterList, deleteAmmeterById } from '../services'
import * as utils from '@/utils/utils'

const FormItem = Form.Item

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    data: [],
    currentPage: 1,
    pageSize: 10,
    totalPage: 0,
    totalRecords: 0
  }

  componentDidMount() {
    const { currentPage, pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  }

  getData({ currentPage, pageSize }) {
    const { validateFields } = this.props.form;
    validateFields((err, values) => {
      if (!err) {
        let obj = {
          currentPage,
          pageSize,
        };

        if (values.ammeterName) {
          obj.ammeterName = values.ammeterName
        }
        const str = utils.queryString(obj)
        searchAmmeterList(str).then(res => {
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
    const { resetFields } = this.props.form;
    resetFields();
    this.getData({ currentPage: 1, pageSize: 10 });
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

  render() {
    const { getFieldDecorator } = this.props.form

    const { data, currentPage, pageSize, totalPage, totalRecords } = this.state
    const columns = [
      {
        title: '电表序号',
        dataIndex: 'id',
        key: 'id'
      }, {
        title: '电表名称',
        dataIndex: 'name',
        key: 'name'
      }, {
        title: '电表编号',
        dataIndex: 'code',
        key: 'code'
      }, {
        title: '状态',
        dataIndex: 'status',
        key: 'status',
        render: (text, row) => text === 0 ? <span style={{ color: '#f5222d' }}>离线</span> : <span style={{ color: '#52c41a' }}>在线</span>
      }, {
        title: '电压倍率',
        dataIndex: 'voltageRate',
        key: 'voltageRate'
      }, {
        title: '电流倍率',
        dataIndex: 'currentRate',
        key: 'currentRate'
      }, {
        title: '用电类型',
        dataIndex: 'electricityType',
        key: 'electricityType',
        render: (text, row) => text === 0 ? '炉变电' : '动力电'
      }, {
        title: '所属工厂',
        dataIndex: 'factoryName',
        key: 'factoryName'
      }, {
        title: ' 创建时间',
        dataIndex: 'createTime',
        key: 'createTime'
      }, {
        title: ' 操作',
        key: 'actions',
        render: (text, row) => (
          <div>
            <span onClick={() => router.push(`/device/ammeter/edit?id=${row.id}`)} style={{ color: '#1890ff', cursor: 'pointer' }}>编辑</span>
            <Divider type="vertical" />
            <Popconfirm
              placement="topRight"
              title={'确认删除该电表？'}
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
        <BreadCrumb extra={<Button type="primary" onClick={() => router.push('/device/ammeter/edit')}>新增</Button>} />
        <div className={styles.container}>
          <Card title="电表管理" bordered={false}>
            <Form layout="inline">
              <Row type="flex" justify="space-between" style={{ width: '100%' }}>
                <Col>
                  <FormItem label="电表搜索">
                    {getFieldDecorator('ammeterName')(
                      <Input style={{ width: 260 }} placeholder="设备名称" autoComplete='off'/>
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
